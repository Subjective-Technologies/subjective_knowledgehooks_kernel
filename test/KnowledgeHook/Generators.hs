{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : KnowledgeHook.Generators  
Description : QuickCheck generators for Knowledge Hook types
Copyright   : (c) 2025 Subjective Technologies
License     : BSD3
Maintainer  : dev@subjectivetechnologies.com

This module provides QuickCheck generators for all Knowledge Hook types,
ensuring well-typed test data that respects invariants and produces
meaningful property tests.
-}

module KnowledgeHook.Generators where

import Test.QuickCheck
import KnowledgeHook.Core
import KnowledgeHook.Semantics (evalCondition)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, fromGregorian, secondsToDiffTime, UTCTime(..))
import Data.Time.Clock (addUTCTime)

-- * Primitive Generators

instance Arbitrary HookId where
  arbitrary = HookId . T.pack <$> listOf1 (choose ('a', 'z'))

instance Arbitrary Alpha where
  arbitrary = Alpha <$> choose (0.01, 0.99)  -- Ensure bounds (0,1)

instance Arbitrary Score where  
  arbitrary = Score <$> choose (0.0, 1.0)

instance Arbitrary Cost where
  arbitrary = Cost <$> choose (0.0, 100.0)

instance Arbitrary Specificity where
  arbitrary = Specificity <$> choose (0.0, 10.0)

instance Arbitrary Correction where
  arbitrary = Correction <$> arbitrary

-- * Feature and Context Generators

instance Arbitrary Feature where
  arbitrary = Feature <$> genText <*> genFeatureType
    where
      genFeatureType = elements ["text", "app", "time", "numeric", "bool"]

instance Arbitrary FeatureValue where
  arbitrary = oneof
    [ TextValue <$> genText
    , AppValue <$> genText  
    , TimeValue <$> genTime
    , NumericValue <$> arbitrary
    , BoolValue <$> arbitrary
    ]

instance Arbitrary Context where
  arbitrary = Context <$> genFeatureMap <*> genTime <*> genMaybeApp
    where
      genFeatureMap = Map.fromList <$> listOf ((,) <$> arbitrary <*> arbitrary)
      genMaybeApp = oneof [pure Nothing, Just <$> genText]

-- * Condition Generators

instance Arbitrary (Condition Context) where
  arbitrary = sized genCondition
    where
      genCondition 0 = oneof [pure TrueP, pure FalseP]
      genCondition n = oneof
        [ pure TrueP
        , pure FalseP  
        , HasText <$> genText
        , InApp <$> genText
        , TimeAfter <$> genTime
        , TimeBefore <$> genTime
        , FeatureEq <$> arbitrary <*> arbitrary
        , AndP <$> genCondition (n `div` 2) <*> genCondition (n `div` 2)
        , OrP <$> genCondition (n `div` 2) <*> genCondition (n `div` 2)
        , NotP <$> genCondition (n - 1)
        ]

-- * Operation Generators

instance Arbitrary Op where
  arbitrary = sized genOp
    where
      genOp 0 = oneof
        [ TypeText <$> genText
        , ClickElement <$> genText
        , OpenApp <$> genText
        , SendKeys <$> genText  
        , Wait <$> choose (0.1, 5.0)
        ]
      genOp n = oneof
        [ TypeText <$> genText
        , ClickElement <$> genText
        , OpenApp <$> genText
        , SendKeys <$> genText
        , Wait <$> choose (0.1, 5.0)
        , Sequence <$> listOf (genOp (n `div` 4))
        ]

instance Arbitrary RollbackPlan where
  arbitrary = RollbackPlan <$> listOf arbitrary <*> arbitrary

instance Arbitrary OutcomePlan where  
  arbitrary = OutcomePlan <$> listOf arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Action where
  arbitrary = Action <$> arbitrary <*> genText

-- * Outcome and Stats Generators

instance Arbitrary Outcome where
  arbitrary = Outcome <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Stats where
  arbitrary = Stats <$> arbitrary <*> genNonNegInt <*> genNonNegInt
    where genNonNegInt = abs <$> arbitrary

instance Arbitrary Metadata where
  arbitrary = Metadata <$> genTime <*> genTime <*> genText <*> listOf genText

-- * Hook Generators

instance Arbitrary Hook where
  arbitrary = Hook 
    <$> arbitrary 
    <*> arbitrary
    <*> arbitrary  
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary

instance Arbitrary HookMatch where
  arbitrary = HookMatch <$> arbitrary <*> arbitrary <*> arbitrary

-- * Policy and Store Generators

instance Arbitrary Policy where
  arbitrary = Policy <$> arbitrary <*> choose (0.1, 2.0) <*> choose (1, 10)

instance Arbitrary HookStore where
  arbitrary = do
    hooks <- listOf arbitrary
    let hookMap = Map.fromList [(hookId h, h) | h <- hooks]
        indices = Map.fromList [(T.pack . show $ hookCondition h, [hookId h]) | h <- hooks]
    return $ HookStore hookMap indices

-- * Snapshot and UserInput Generators

instance Arbitrary Snapshot where
  arbitrary = Snapshot <$> arbitrary <*> genTime <*> genText

instance Arbitrary UserInput where  
  arbitrary = UserInput <$> listOf arbitrary <*> listOf genText <*> genTime

instance Arbitrary Equiv where
  arbitrary = elements [Equivalent, NotEquivalent]

-- * Utility Generators

genText :: Gen Text
genText = T.pack <$> listOf1 (choose ('a', 'z'))

genTime :: Gen UTCTime  
genTime = do
  year <- choose (2020, 2030)
  month <- choose (1, 12)
  day <- choose (1, 28)  -- Safe for all months
  seconds <- choose (0, 86399)  -- Seconds in a day
  return $ UTCTime (fromGregorian year month day) (secondsToDiffTime seconds)

-- * Constrained Generators

-- | Generate valid Alpha in (0,1)
genValidAlpha :: Gen Alpha
genValidAlpha = Alpha <$> choose (0.001, 0.999)

-- | Generate hooks with matching conditions for equivalence tests
genEquivalentHooks :: Gen (Hook, Hook)
genEquivalentHooks = do
  baseHook <- arbitrary
  let equivalentHook = baseHook { hookId = HookId "equivalent_copy" }
  return (baseHook, equivalentHook)

-- | Generate hooks with different conditions for refinement tests  
genRefinementPair :: Gen (Hook, Hook)
genRefinementPair = do
  general <- arbitrary
  specific <- arbitrary
  let refinedCondition = andP (hookCondition general) (hookCondition specific)
      specificHook = specific { hookCondition = refinedCondition }
  return (general, specificHook)

-- | Generate context that satisfies a condition
genSatisfyingContext :: Condition Context -> Gen Context
genSatisfyingContext condition = arbitrary `suchThat` evalCondition condition

-- | Generate non-empty list of hook matches
genNonEmptyMatches :: Gen [HookMatch]  
genNonEmptyMatches = listOf1 arbitrary

-- | Generate hooks with same cost for tie-breaking tests
genSameCostMatches :: Gen (HookMatch, HookMatch)
genSameCostMatches = do
  cost <- arbitrary
  m1 <- arbitrary
  m2 <- arbitrary  
  return (m1 { matchCost = cost }, m2 { matchCost = cost })

