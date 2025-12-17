{-|
Module      : KnowledgeHook.Interpret
Description : Pure interpreters for Knowledge Hook actions
Copyright   : (c) 2025 Subjective Technologies
License     : BSD3
Maintainer  : dev@subjectivetechnologies.com

This module provides pure interpreters for Knowledge Hook actions and rollback
plans. The interpreters are total functions that transform contexts according
to the action semantics, maintaining referential transparency.
-}

module KnowledgeHook.Interpret
  ( -- * Interpreters
    interpretAction
  , interpretRollback
  , interpretOp
  , interpretOutcomePlan
    -- * Context Transformations  
  , applyOp
  , applyOpSequence
  , simulateExecution
    -- * Rollback Support
  , createContextSnapshot
  , restoreContext
  ) where

import KnowledgeHook.Types
import qualified Data.Map.Strict as Map
import Data.Time (addUTCTime)

-- | Interpret an action in a context, returning new context and rollback plan (total)
-- Total: delegates to total functions interpretOutcomePlan
interpretAction :: Action -> Context -> (Context, RollbackPlan)
interpretAction action ctx = 
  let plan = actionPlan action
      (newCtx, rollback) = interpretOutcomePlan plan ctx
  in (newCtx, rollback)

-- | Interpret a rollback plan to restore previous context (total)
-- Total: pattern match on RollbackPlan is exhaustive (single constructor)
interpretRollback :: RollbackPlan -> Context -> Context
interpretRollback (RollbackPlan ops targetCtx) currentCtx =
  -- Apply rollback operations and restore target context
  let contextAfterRollback = foldl (flip applyOp) currentCtx ops
  in mergeContexts targetCtx contextAfterRollback

-- | Merge two contexts, preferring the target for restoration (total)
-- Total: record update is total, all fields defined
mergeContexts :: Context -> Context -> Context
mergeContexts target current = target
  { contextTime = contextTime current  -- Keep current time
  }

-- | Interpret an outcome plan in a context (total)
-- Total: pattern match on OutcomePlan is exhaustive (single constructor)
interpretOutcomePlan :: OutcomePlan -> Context -> (Context, RollbackPlan)
interpretOutcomePlan (OutcomePlan ops rollback _cost) ctx =
  let newCtx = applyOpSequence ops ctx
      snapshotRollback = rollback { rollbackContext = ctx }
  in (newCtx, snapshotRollback)

-- | Interpret a single operation (total)
-- Total: delegates to total function applyOp
interpretOp :: Op -> Context -> Context
interpretOp = applyOp

-- | Apply a single operation to a context (total)
-- Total: exhaustive pattern match covers all Op constructors
applyOp :: Op -> Context -> Context
applyOp op ctx = case op of
  TypeText txt -> 
    -- Add text to context features
    let textFeature = Feature "typed_text" "text"
        newFeatures = Map.insert textFeature (TextValue txt) (contextFeatures ctx)
    in ctx { contextFeatures = newFeatures }
    
  ClickElement element ->
    -- Record clicked element
    let clickFeature = Feature "clicked_element" "ui"
        newFeatures = Map.insert clickFeature (TextValue element) (contextFeatures ctx)
    in ctx { contextFeatures = newFeatures }
    
  OpenApp app ->
    -- Change current app context
    ctx { contextApp = Just app }
    
  SendKeys keys ->
    -- Similar to TypeText but for key sequences
    let keysFeature = Feature "sent_keys" "input" 
        newFeatures = Map.insert keysFeature (TextValue keys) (contextFeatures ctx)
    in ctx { contextFeatures = newFeatures }
    
  Wait duration ->
    -- Advance time
    let newTime = addUTCTime (realToFrac duration) (contextTime ctx)
    in ctx { contextTime = newTime }
    
  Sequence ops ->
    -- Apply operations in sequence
    applyOpSequence ops ctx

-- | Apply a sequence of operations (total)
-- Total: foldl with total function applyOp over finite list
applyOpSequence :: [Op] -> Context -> Context
applyOpSequence ops ctx = foldl (flip applyOp) ctx ops

-- | Simulate execution of a hook in a context (pure, total)
-- Total: delegates to total function interpretAction, constructs total Outcome
simulateExecution :: Hook -> Context -> (Context, Outcome)
simulateExecution hook ctx = 
  let action = hookAction hook
      (newCtx, _rollback) = interpretAction action ctx
      outcome = Outcome newCtx (Correction False) (actionPlan action)
  in (newCtx, outcome)

-- | Create a snapshot of current context for rollback (total)
-- Total: identity function on immutable data
createContextSnapshot :: Context -> Context
createContextSnapshot = id  -- Context is already immutable

-- | Restore context from snapshot (total)
-- Total: ignores first argument, returns second (contexts are immutable)
restoreContext :: Context -> Context -> Context
restoreContext _current snapshot = snapshot
