{-# LANGUAGE LambdaCase #-}

{-|
Module      : KnowledgeHook.Algebra
Description : Algebraic operations on Knowledge Hooks  
Copyright   : (c) 2025 Subjective Technologies
License     : BSD3
Maintainer  : dev@subjectivetechnologies.com

This module implements the core algebraic operations from the Knowledge Hook
algebra, including composition, refinement, equivalence, and learning updates.
All operations are total and maintain algebraic laws.
-}

module KnowledgeHook.Algebra
  ( -- * Core Operations
    activate
  , prioritize  
  , execute
  , learnDelta
    -- * Composition
  , composeNested
  , composeFlat
  , cascade
    -- * Refinement & Equivalence  
  , refine
  , equivalent
  , specificity
    -- * Rollback
  , rollback
  , createRollbackPlan
    -- * Weight Normalization
  , normalizeWeights
  , combineHooks
    -- * Learning Updates
  , updateStats
  , negativeRLUpdate
    -- * Utilities
  , estimateCost
  , calculateSpecificity
  ) where

import KnowledgeHook.Types
import KnowledgeHook.Semantics
import KnowledgeHook.Semantics (normalizeCondition)
import KnowledgeHook.Interpret (interpretAction, interpretRollback)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.List (sortOn, minimumBy)
import Data.Ord (comparing)
import Data.Time (UTCTime, getCurrentTime, addUTCTime)
import Control.Monad (guard)

-- | Activate hooks whose conditions hold in the given context (total)
-- Total: list comprehension over finite map, evalCondition is total
activate :: HookStore -> Context -> [HookMatch]
activate store ctx = 
  [ HookMatch hook ctx (estimateCost (hookAction hook) ctx)
  | hook <- Map.elems (storeHooks store)
  , evalCondition (hookCondition hook) ctx
  ]

-- | Prioritize hooks according to minimization law and success score tie-breaking (total)
-- Total: exhaustive pattern match on list, minimumBy on non-empty list
prioritize :: Policy -> [HookMatch] -> Maybe HookMatch
prioritize _policy [] = Nothing
prioritize policy matches = Just $ minimumBy compareMatches matches
  where
    compareMatches m1 m2 = 
      case comparing matchCost m1 m2 of
        EQ -> comparing (negate . unScore . statsSuccess . hookStats . matchHook) m1 m2
        other -> other

-- | Execute a hook in a context to produce an outcome (pure, total)
-- Total: delegates to total interpretAction, constructs total Outcome
execute :: Hook -> Context -> Outcome
execute hook ctx = 
  let (newCtx, _rb) = interpretAction (hookAction hook) ctx
  in Outcome newCtx (Correction False) (actionPlan (hookAction hook))

-- | Learn from a delta between snapshots with user input (total)
-- Total: delegates to total helper functions, constructs total Hook
learnDelta :: Alpha -> Snapshot -> Snapshot -> UserInput -> HookStore -> HookStore
learnDelta alpha before after userInput store =
  let deltaCondition = inferCondition (snapshotContext before) (snapshotContext after)
      deltaAction = inferAction userInput
      hookId' = HookId ("learned_" <> snapshotId after)
      newHook = Hook
        { hookId = hookId'
        , hookCondition = deltaCondition
        , hookAction = deltaAction
        , hookStats = Stats (Score 0.5) 0 0  -- Initial neutral score
        , hookMeta = Metadata 
            (inputTime userInput) 
            (inputTime userInput)
            "learned" 
            ["delta"]
        , hookSpecificity = calculateSpecificity deltaCondition
        }
  in insertHookInStore newHook store
  where
    insertHookInStore hook store' = store'
      { storeHooks = Map.insert (hookId hook) hook (storeHooks store')
      , storeIndices = 
          let k = T.pack . show . normalizeCondition $ hookCondition hook
          in Map.insertWith (<>) k [hookId hook] (storeIndices store')
      }

-- | Infer condition from context delta (simplified heuristic, total)
-- Total: exhaustive pattern match with default case, Maybe pattern match total
inferCondition :: Context -> Context -> Condition Context
inferCondition before after
  | contextApp before /= contextApp after = 
      case contextApp after of
        Just app -> inApp app
        Nothing -> TrueP
  | otherwise = TrueP  -- Default: total fallback for unanalyzed changes

-- | Infer action from user input (total)
-- Total: constructs total Action from total components
inferAction :: UserInput -> Action
inferAction userInput = Action
  { actionPlan = OutcomePlan 
      (inputActions userInput) 
      (createRollbackPlan (inputActions userInput))
      (Cost $ fromIntegral $ length $ inputActions userInput)
  , actionDescription = "Learned from user input"
  }

-- | Create rollback plan for a sequence of operations (total)
-- Total: exhaustive pattern match on Op, map and reverse are total on finite lists
createRollbackPlan :: [Op] -> RollbackPlan
createRollbackPlan ops = RollbackPlan (reverse $ map invertOp ops) emptyContext
  where
         invertOp = \case
           TypeText txt -> SendKeys (T.pack $ replicate (T.length txt) '\b')  -- Backspaces
           ClickElement elem -> ClickElement elem  -- Click again (toggle)
           OpenApp _app -> Wait 0  -- Can't easily undo app opening (safe default)
           SendKeys keys -> SendKeys (T.pack $ replicate (T.length keys) '\b')
           Wait _t -> Wait 0  -- No-op wait (safe default)
           Sequence ops' -> Sequence (reverse $ map invertOp ops')

-- | Nested composition: h2 executes in the outcome context of h1
composeNested :: Hook -> Hook -> Hook
composeNested h1 h2 = Hook
  { hookId = HookId (unHookId (hookId h1) <> "_then_" <> unHookId (hookId h2))
  , hookCondition = andP (hookCondition h1) (hookCondition h2)  -- Both must hold
  , hookAction = Action
      { actionPlan = actionPlan (hookAction h1) <> actionPlan (hookAction h2)
      , actionDescription = actionDescription (hookAction h1) <> " then " <> actionDescription (hookAction h2)
      }
  , hookStats = combineStats (hookStats h1) (hookStats h2)
  , hookMeta = combineMetadata (hookMeta h1) (hookMeta h2)
  , hookSpecificity = hookSpecificity h1 + hookSpecificity h2
  }

-- | Flat composition: execute h1 and h2 in parallel/sequence
composeFlat :: Hook -> Hook -> Hook  
composeFlat h1 h2 = Hook
  { hookId = HookId (unHookId (hookId h1) <> "_and_" <> unHookId (hookId h2))
  , hookCondition = orP (hookCondition h1) (hookCondition h2)  -- Either can trigger
  , hookAction = Action
      { actionPlan = actionPlan (hookAction h1) <> actionPlan (hookAction h2)
      , actionDescription = actionDescription (hookAction h1) <> " and " <> actionDescription (hookAction h2)  
      }
  , hookStats = combineStats (hookStats h1) (hookStats h2)
  , hookMeta = combineMetadata (hookMeta h1) (hookMeta h2)
  , hookSpecificity = min (hookSpecificity h1) (hookSpecificity h2)
  }

-- | Cascade: find hooks triggered by an outcome
cascade :: Policy -> HookStore -> Outcome -> [Hook]
cascade policy store outcome = 
  let matches = activate store (outcomePost outcome)
      cascadeDepth = policyMaxCascadeDepth policy
  in if cascadeDepth > 0
     then case prioritize policy matches of
       Just match -> [matchHook match]  -- Simplified: take top match
       Nothing -> []
     else []

-- | Refine a hook to be more specific
refine :: Hook -> Hook -> Hook
refine general specific = specific
  { hookCondition = andP (hookCondition general) (hookCondition specific)
  , hookSpecificity = hookSpecificity general + hookSpecificity specific
  }

-- | Check equivalence of two hooks using normalized conditions
equivalent :: Hook -> Hook -> Equiv
equivalent h1 h2
  | normalizeCondition (hookCondition h1) == normalizeCondition (hookCondition h2) && 
    actionPlan (hookAction h1) == actionPlan (hookAction h2) = Equivalent
  | otherwise = NotEquivalent

-- | Calculate specificity of a condition (total)
-- Total: exhaustive pattern match covers all Condition constructors
calculateSpecificity :: Condition Context -> Specificity
calculateSpecificity = \case
  TrueP -> Specificity 0
  FalseP -> Specificity 1  -- Most specific (never matches)
  HasText _ -> Specificity 0.3
  InApp _ -> Specificity 0.4
  TimeAfter _ -> Specificity 0.2
  TimeBefore _ -> Specificity 0.2
  FeatureEq _ _ -> Specificity 0.5
  AndP c1 c2 -> calculateSpecificity c1 + calculateSpecificity c2
  OrP c1 c2 -> min (calculateSpecificity c1) (calculateSpecificity c2)
  NotP c -> calculateSpecificity c

-- | Get specificity of a hook (total)
-- Total: record field access is total
specificity :: Hook -> Specificity
specificity = hookSpecificity

-- | Apply rollback plan to undo an outcome (total)
-- Total: delegates to total interpretRollback function
rollback :: Outcome -> Context -> Context
rollback outcome ctx = 
  interpretRollback (planRollback (outcomePlan outcome)) ctx

-- | Normalize weights between similar hooks
normalizeWeights :: Hook -> Hook -> (Hook, Hook)
normalizeWeights h1 h2 = 
  let totalUses = statsUses (hookStats h1) + statsUses (hookStats h2)
      weight1 = if totalUses > 0 then fromIntegral (statsUses (hookStats h1)) / fromIntegral totalUses else 0.5
      weight2 = 1.0 - weight1
      newScore1 = Score $ weight1 * unScore (statsSuccess (hookStats h1))
      newScore2 = Score $ weight2 * unScore (statsSuccess (hookStats h2))
  in ( h1 { hookStats = (hookStats h1) { statsSuccess = newScore1 } }
     , h2 { hookStats = (hookStats h2) { statsSuccess = newScore2 } }
     )

-- | Combine two hooks into one
combineHooks :: Hook -> Hook -> Hook
combineHooks h1 h2 = composeFlat h1 h2

-- | Update hook statistics after execution
updateStats :: Alpha -> Hook -> Correction -> Hook
updateStats alpha hook correction = hook
  { hookStats = newStats
  }
  where
    oldStats = hookStats hook
    s' = negativeRLUpdate alpha (statsSuccess oldStats) correction
    newStats = Stats
      { statsSuccess = s'
      , statsUses = statsUses oldStats + 1
      , statsCorrections = statsCorrections oldStats + if unCorrection correction then 1 else 0
      }

-- | Apply negative reinforcement learning update
-- success' = (1 - α) * success + α * (1 - correctionFlag)
negativeRLUpdate :: Alpha -> Score -> Correction -> Score
negativeRLUpdate alpha oldScore correction =
  let α = unAlpha alpha
      s = unScore oldScore
      c = if unCorrection correction then 1.0 else 0.0
      newScore = (1 - α) * s + α * (1 - c)
  in Score newScore

-- | Estimate cost of executing an action in a context
estimateCost :: Action -> Context -> Cost
estimateCost action _ctx = planCost (actionPlan action)

-- | Combine statistics from two hooks
combineStats :: Stats -> Stats -> Stats
combineStats s1 s2 = Stats
  { statsSuccess = Score $ (unScore (statsSuccess s1) + unScore (statsSuccess s2)) / 2
  , statsUses = statsUses s1 + statsUses s2
  , statsCorrections = statsCorrections s1 + statsCorrections s2
  }

-- | Combine metadata from two hooks  
combineMetadata :: Metadata -> Metadata -> Metadata
combineMetadata m1 m2 = Metadata
  { metaCreated = min (metaCreated m1) (metaCreated m2)
  , metaModified = max (metaModified m1) (metaModified m2)
  , metaSource = "composed"
  , metaTags = metaTags m1 <> metaTags m2
  }
