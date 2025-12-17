{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

{-|
Module      : KnowledgeHook.Semantics  
Description : Denotational semantics for Knowledge Hooks
Copyright   : (c) 2025 Subjective Technologies
License     : BSD3
Maintainer  : dev@subjectivetechnologies.com

This module provides the denotational semantics for Knowledge Hooks, defining
the mathematical meaning of each construct through total functions.

The semantics follow the pattern:
⟦Condition⟧ :: Context -> Bool  
⟦Action⟧ :: Context -> OutcomePlan
⟦Hook⟧ :: Context -> Maybe OutcomePlan

All functions are total and referentially transparent.
-}

module KnowledgeHook.Semantics
  ( -- * Denotational Semantics
    evalCondition
  , evalAction  
  , evalHook
  , evalOutcomePlan
    -- * Condition Combinators
  , andP
  , orP
  , notP
  , hasText
  , inApp
  , timeAfter
  , timeBefore
  , featureEq
    -- * Condition Normalization
  , normalizeCondition
    -- * Context Utilities
  , contextHasText
  , contextInApp
  , contextFeature
  ) where

import KnowledgeHook.Types
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime)
import Data.List (sort)

-- | ⟦Condition⟧ :: Context -> Bool (total)
-- Denotational semantics for conditions as predicates over contexts
-- Total: exhaustive pattern match covers all Condition constructors
evalCondition :: Condition Context -> Context -> Bool
evalCondition = \case
  TrueP -> const True
  FalseP -> const False
  HasText txt -> contextHasText txt
  InApp app -> contextInApp app  
  TimeAfter t -> \ctx -> contextTime ctx > t
  TimeBefore t -> \ctx -> contextTime ctx < t
  FeatureEq feat val -> \ctx -> contextFeature feat ctx == Just val
  AndP c1 c2 -> \ctx -> evalCondition c1 ctx && evalCondition c2 ctx
  OrP c1 c2 -> \ctx -> evalCondition c1 ctx || evalCondition c2 ctx
  NotP c -> \ctx -> not (evalCondition c ctx)

-- | ⟦Action⟧ :: Context -> OutcomePlan (total)
-- Denotational semantics for actions as pure plans
-- Total: record field access is total
evalAction :: Action -> Context -> OutcomePlan
evalAction action _ctx = actionPlan action

-- | ⟦Hook⟧ :: Context -> Maybe OutcomePlan (total)
-- Denotational semantics for hooks: activate if condition holds
-- Total: delegates to total evalCondition and evalAction
evalHook :: Hook -> Context -> Maybe OutcomePlan  
evalHook hook ctx
  | evalCondition (hookCondition hook) ctx = Just (evalAction (hookAction hook) ctx)
  | otherwise = Nothing

-- | Evaluate an outcome plan in a context (for cost estimation, total)
-- Total: record field access is total
evalOutcomePlan :: OutcomePlan -> Context -> Cost
evalOutcomePlan plan _ctx = planCost plan

-- * Condition Combinators

-- | Smart constructor for conjunction
andP :: Condition a -> Condition a -> Condition a
andP TrueP c = c
andP c TrueP = c  
andP FalseP _ = FalseP
andP _ FalseP = FalseP
andP c1 c2 = AndP c1 c2

-- | Smart constructor for disjunction  
orP :: Condition a -> Condition a -> Condition a
orP TrueP _ = TrueP
orP _ TrueP = TrueP
orP FalseP c = c
orP c FalseP = c
orP c1 c2 = OrP c1 c2

-- | Smart constructor for negation
notP :: Condition a -> Condition a
notP TrueP = FalseP
notP FalseP = TrueP
notP (NotP c) = c  -- Double negation elimination
notP c = NotP c

-- | Smart constructor for text presence
hasText :: Text -> Condition Context
hasText = HasText

-- | Smart constructor for app context
inApp :: Text -> Condition Context  
inApp = InApp

-- | Smart constructor for time constraints
timeAfter :: UTCTime -> Condition Context
timeAfter = TimeAfter

timeBefore :: UTCTime -> Condition Context  
timeBefore = TimeBefore

-- | Smart constructor for feature equality
featureEq :: Feature -> FeatureValue -> Condition Context
featureEq = FeatureEq

-- * Condition Normalization

-- | Normalize a condition to canonical form for robust structural equality
-- Rules applied in order:
-- 1. Push negations inward (De Morgan's laws)
-- 2. Eliminate TrueP/FalseP identities and absorbing elements
-- 3. Flatten associative operations (AndP/OrP)
-- 4. Sort subterms deterministically for canonical ordering
normalizeCondition :: Condition Context -> Condition Context
normalizeCondition = sortCondition . flattenCondition . eliminateIdentities . pushNegations
  where
    -- Push negations inward using De Morgan's laws
    pushNegations :: Condition Context -> Condition Context
    pushNegations = \case
      NotP (NotP c) -> pushNegations c  -- Double negation elimination
      NotP (AndP c1 c2) -> OrP (pushNegations (NotP c1)) (pushNegations (NotP c2))  -- De Morgan
      NotP (OrP c1 c2) -> AndP (pushNegations (NotP c1)) (pushNegations (NotP c2))   -- De Morgan
      NotP TrueP -> FalseP
      NotP FalseP -> TrueP
      AndP c1 c2 -> AndP (pushNegations c1) (pushNegations c2)
      OrP c1 c2 -> OrP (pushNegations c1) (pushNegations c2)
      c -> c  -- Atomic conditions unchanged
    
    -- Eliminate identities and absorbing elements
    eliminateIdentities :: Condition Context -> Condition Context
    eliminateIdentities = \case
      AndP TrueP c -> eliminateIdentities c
      AndP c TrueP -> eliminateIdentities c
      AndP FalseP _ -> FalseP
      AndP _ FalseP -> FalseP
      OrP FalseP c -> eliminateIdentities c
      OrP c FalseP -> eliminateIdentities c
      OrP TrueP _ -> TrueP
      OrP _ TrueP -> TrueP
      AndP c1 c2 -> 
        let c1' = eliminateIdentities c1
            c2' = eliminateIdentities c2
        in case (c1', c2') of
          (TrueP, c) -> c
          (c, TrueP) -> c
          (FalseP, _) -> FalseP
          (_, FalseP) -> FalseP
          _ -> AndP c1' c2'
      OrP c1 c2 -> 
        let c1' = eliminateIdentities c1
            c2' = eliminateIdentities c2
        in case (c1', c2') of
          (FalseP, c) -> c
          (c, FalseP) -> c
          (TrueP, _) -> TrueP
          (_, TrueP) -> TrueP
          _ -> OrP c1' c2'
      c -> c
    
    -- Flatten nested associative operations
    flattenCondition :: Condition Context -> Condition Context
    flattenCondition = \case
      AndP c1 c2 -> 
        let flats1 = flattenAnds (flattenCondition c1)
            flats2 = flattenAnds (flattenCondition c2)
        in buildAnd (flats1 ++ flats2)
      OrP c1 c2 ->
        let flats1 = flattenOrs (flattenCondition c1)
            flats2 = flattenOrs (flattenCondition c2)
        in buildOr (flats1 ++ flats2)
      c -> c
    
    -- Extract all And terms into a flat list
    flattenAnds :: Condition Context -> [Condition Context]
    flattenAnds = \case
      AndP c1 c2 -> flattenAnds c1 ++ flattenAnds c2
      c -> [c]
    
    -- Extract all Or terms into a flat list
    flattenOrs :: Condition Context -> [Condition Context]
    flattenOrs = \case
      OrP c1 c2 -> flattenOrs c1 ++ flattenOrs c2
      c -> [c]
    
    -- Build And from flat list
    buildAnd :: [Condition Context] -> Condition Context
    buildAnd [] = TrueP
    buildAnd [c] = c
    buildAnd cs = foldr1 AndP cs
    
    -- Build Or from flat list  
    buildOr :: [Condition Context] -> Condition Context
    buildOr [] = FalseP
    buildOr [c] = c
    buildOr cs = foldr1 OrP cs
    
    -- Sort condition terms for canonical ordering
    sortCondition :: Condition Context -> Condition Context
    sortCondition = \case
      AndP c1 c2 ->
        let sorted = sort [sortCondition c1, sortCondition c2]
        in case sorted of
          [x, y] -> AndP x y
          _ -> AndP (sortCondition c1) (sortCondition c2)
      OrP c1 c2 ->
        let sorted = sort [sortCondition c1, sortCondition c2]
        in case sorted of
          [x, y] -> OrP x y
          _ -> OrP (sortCondition c1) (sortCondition c2)
      c -> c

-- Need Ord instance for Condition to enable sorting
instance Ord (Condition Context) where
  compare c1 c2 = compare (conditionPriority c1) (conditionPriority c2) <> compare (show c1) (show c2)
    where
      conditionPriority :: Condition Context -> Int
      conditionPriority = \case
        TrueP -> 0
        FalseP -> 1
        HasText _ -> 2
        InApp _ -> 3
        TimeAfter _ -> 4
        TimeBefore _ -> 5
        FeatureEq _ _ -> 6
        NotP _ -> 7
        AndP _ _ -> 8
        OrP _ _ -> 9

-- * Context Utilities

-- | Check if context contains specific text
contextHasText :: Text -> Context -> Bool
contextHasText txt ctx = any hasTextValue (Map.elems (contextFeatures ctx))
  where
    hasTextValue (TextValue t) = txt `T.isInfixOf` t
    hasTextValue _ = False

-- | Check if context is in specific app
contextInApp :: Text -> Context -> Bool  
contextInApp app ctx = case contextApp ctx of
  Just currentApp -> app == currentApp
  Nothing -> False

-- | Get feature value from context
contextFeature :: Feature -> Context -> Maybe FeatureValue
contextFeature feat ctx = Map.lookup feat (contextFeatures ctx)
