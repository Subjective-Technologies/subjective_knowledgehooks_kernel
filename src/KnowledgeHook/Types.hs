{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-|
Module      : KnowledgeHook.Types
Description : Core algebraic types for Knowledge Hooks
Copyright   : (c) 2025 Subjective Technologies
License     : BSD3
Maintainer  : dev@subjectivetechnologies.com

This module defines the fundamental algebraic types that form the basis of the
Knowledge Hook algebra, following the mathematical framework from the reference.
All types are designed to be total and encode invariants through the type system.
-}

module KnowledgeHook.Types
  ( -- * Core Types
    Context(..)
  , Condition(..)
  , Action(..)
  , Outcome(..)
  , Hook(..)
    -- * Primitive Types  
  , HookId(..)
  , Alpha(..)
  , Score(..)
  , Cost(..)
  , Specificity(..)
  , Correction(..)
  , Stats(..)
  , Metadata(..)
    -- * Operations
  , Op(..)
  , RollbackPlan(..)
  , OutcomePlan(..)
    -- * Storage
  , HookStore(..)
  , HookMatch(..)
  , Policy(..)
  , Snapshot(..)
  , UserInput(..)
  , Equiv(..)
    -- * Feature Space
  , Feature(..)
  , FeatureValue(..)
    -- * Smart Constructors
  , emptyContext
  ) where

import Data.Text (Text)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Time (UTCTime, UTCTime(..), fromGregorian)

-- | Unique identifier for hooks
newtype HookId = HookId { unHookId :: Text }
  deriving (Eq, Ord, Show)
  deriving newtype (Semigroup, Monoid)

-- | Learning rate parameter α ∈ (0,1) for negative reinforcement learning
newtype Alpha = Alpha { unAlpha :: Double }
  deriving (Eq, Ord, Show)
  deriving newtype (Num, Fractional)

-- | Success score ∈ [0,1]
newtype Score = Score { unScore :: Double }
  deriving (Eq, Ord, Show)
  deriving newtype (Num, Fractional)

-- | Expected user input cost (lower is better)
newtype Cost = Cost { unCost :: Double }
  deriving (Eq, Ord, Show)
  deriving newtype (Num, Fractional)

-- | Specificity measure for refinement ordering
newtype Specificity = Specificity { unSpecificity :: Double }
  deriving (Eq, Ord, Show)
  deriving newtype (Num, Fractional)

-- | Correction flag - True indicates user correction was needed
newtype Correction = Correction { unCorrection :: Bool }
  deriving (Eq, Ord, Show)
  deriving newtype (Enum)

-- | Feature in the context space
data Feature = Feature
  { featureName :: Text
  , featureType :: Text  -- "text", "app", "time", etc.
  } deriving (Eq, Ord, Show)

-- | Values that features can take
data FeatureValue 
  = TextValue Text
  | AppValue Text  
  | TimeValue UTCTime
  | NumericValue Double
  | BoolValue Bool
  deriving (Eq, Ord, Show)

-- | Context represents the current state/snapshot with features
data Context = Context
  { contextFeatures :: Map Feature FeatureValue
  , contextTime :: UTCTime
  , contextApp :: Maybe Text
  } deriving (Eq, Show)

-- | Condition is a predicate over Context with Boolean algebra structure
data Condition a where
  TrueP :: Condition a
  FalseP :: Condition a
  HasText :: Text -> Condition Context
  InApp :: Text -> Condition Context
  TimeAfter :: UTCTime -> Condition Context
  TimeBefore :: UTCTime -> Condition Context
  FeatureEq :: Feature -> FeatureValue -> Condition Context
  AndP :: Condition a -> Condition a -> Condition a
  OrP :: Condition a -> Condition a -> Condition a
  NotP :: Condition a -> Condition a

deriving instance Eq (Condition a)
deriving instance Show (Condition a)

-- | Atomic operations that can be performed
data Op 
  = TypeText Text
  | ClickElement Text
  | OpenApp Text
  | SendKeys Text
  | Wait Double
  | Sequence [Op]
  deriving (Eq, Show)

-- | Plan for rolling back an operation
data RollbackPlan = RollbackPlan
  { rollbackOps :: [Op]
  , rollbackContext :: Context  -- Context to restore
  } deriving (Eq, Show)

-- | Plan of operations with guaranteed rollback capability
data OutcomePlan = OutcomePlan
  { planOps :: [Op]
  , planRollback :: RollbackPlan
  , planCost :: Cost
  } deriving (Eq, Show)

-- | Semigroup instance for OutcomePlan (sequential composition)
instance Semigroup OutcomePlan where
  OutcomePlan ops1 rb1 cost1 <> OutcomePlan ops2 rb2 cost2 = 
    OutcomePlan 
      (ops1 <> ops2) 
      (rb2 <> rb1)  -- Rollback in reverse order
      (cost1 + cost2)

-- | Monoid instance with empty plan as identity
instance Monoid OutcomePlan where
  mempty = OutcomePlan [] mempty (Cost 0)

-- | Semigroup for RollbackPlan (reverse composition)
instance Semigroup RollbackPlan where
  RollbackPlan ops1 ctx1 <> RollbackPlan ops2 _ctx2 =
    RollbackPlan (ops2 <> ops1) ctx1  -- First context wins

-- | Empty context for use as default (total constructor)
-- Uses Unix epoch as a safe, well-defined time value
emptyContext :: Context
emptyContext = Context Map.empty epochTime Nothing
  where
    epochTime = UTCTime (fromGregorian 1970 1 1) 0  -- Total: Unix epoch

-- | Monoid for RollbackPlan with empty rollback
instance Monoid RollbackPlan where
  mempty = RollbackPlan [] emptyContext

-- | Action represents a pure plan of operations using Free monad structure
data Action = Action
  { actionPlan :: OutcomePlan
  , actionDescription :: Text
  } deriving (Eq, Show)

-- | Outcome represents the result of executing an action
data Outcome = Outcome
  { outcomePost :: Context
  , outcomeCorrection :: Correction
  , outcomePlan :: OutcomePlan  -- What was actually executed
  } deriving (Eq, Show)

-- | Statistics for a hook's performance
data Stats = Stats
  { statsSuccess :: Score
  , statsUses :: Int
  , statsCorrections :: Int
  } deriving (Eq, Show)

-- | Metadata for additional hook information
data Metadata = Metadata
  { metaCreated :: UTCTime
  , metaModified :: UTCTime
  , metaSource :: Text  -- "learned", "predefined", etc.
  , metaTags :: [Text]
  } deriving (Eq, Show)

-- | A Knowledge Hook with all components
data Hook = Hook
  { hookId :: HookId
  , hookCondition :: Condition Context
  , hookAction :: Action
  , hookStats :: Stats
  , hookMeta :: Metadata
  , hookSpecificity :: Specificity
  } deriving (Eq, Show)

-- | A matched hook with its activation context
data HookMatch = HookMatch
  { matchHook :: Hook
  , matchContext :: Context
  , matchCost :: Cost
  } deriving (Eq, Show)

-- | Policy for hook selection and execution
data Policy = Policy
  { policyMinimizeInput :: Bool  -- Prioritize lower user input cost
  , policySuccessWeight :: Double  -- Weight for success score in tie-breaking
  , policyMaxCascadeDepth :: Int  -- Prevent infinite cascading
  } deriving (Eq, Show)

-- | Hook storage abstraction (pure)
data HookStore = HookStore
  { storeHooks :: Map HookId Hook
  , storeIndices :: Map Text [HookId]  -- Index by condition text for fast lookup
  } deriving (Eq, Show)

-- | Snapshot represents a complete state capture
data Snapshot = Snapshot
  { snapshotContext :: Context
  , snapshotTime :: UTCTime
  , snapshotId :: Text
  } deriving (Eq, Show)

-- | User input during learning
data UserInput = UserInput
  { inputActions :: [Op]
  , inputCorrections :: [Text]  -- Descriptions of what was wrong
  , inputTime :: UTCTime
  } deriving (Eq, Show)

-- | Equivalence relation result
data Equiv = Equivalent | NotEquivalent
  deriving (Eq, Show, Enum)

-- Boolean algebra instance for Condition
instance Semigroup (Condition a) where
  (<>) = AndP

instance Monoid (Condition a) where
  mempty = TrueP
