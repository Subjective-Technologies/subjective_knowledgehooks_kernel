{-|
Module      : KnowledgeHook.Core
Description : Main API for the Knowledge Hook kernel
Copyright   : (c) 2025 Subjective Technologies  
License     : BSD3
Maintainer  : dev@subjectivetechnologies.com

This module provides the main public API for the Knowledge Hook kernel,
exposing the core engine and all algebraic operations in a clean interface.
-}

module KnowledgeHook.Core
  ( -- * Core Types (re-exported)
    Context(..)
  , Condition(..)  
  , Action(..)
  , Outcome(..)
  , Hook(..)
  , HookId(..)
  , Alpha(..)
  , Score(..)
  , Cost(..)
  , Specificity(..)
  , Correction(..)
  , Stats(..)
  , Metadata(..)
  , Op(..)
  , RollbackPlan(..)
  , OutcomePlan(..)
  , HookStore(..)
  , HookMatch(..)
  , Policy(..)
  , Snapshot(..)
  , UserInput(..)
  , Equiv(..)
  , Feature(..)
  , FeatureValue(..)
    -- * Engine
  , Engine(..)
  , newEngine
  , engineWithPolicy
    -- * Core Operations
  , activate
  , prioritize
  , execute  
  , learnDelta
    -- * Algebraic Operations
  , composeNested
  , composeFlat
  , cascade
  , refine
  , equivalent
  , rollback
  , normalizeWeights
    -- * Condition Combinators
  , andP
  , orP
  , notP
  , hasText
  , inApp
  , timeAfter
  , timeBefore
  , featureEq
    -- * Store Operations
  , emptyStore
  , insertHook
  , lookupHook
  , deleteHook
  , updateHook
    -- * Utilities
  , mkAlpha
  , mkScore  
  , mkCost
  , mkSpecificity
  , defaultPolicy
  , evalHook
  ) where

import KnowledgeHook.Types
import KnowledgeHook.Semantics
import KnowledgeHook.Algebra
import KnowledgeHook.Semantics (evalHook, normalizeCondition)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Data.Time (UTCTime, getCurrentTime)

-- | Main engine containing hooks, learning parameters, and policy
data Engine = Engine
  { engineHooks :: HookStore
  , engineAlpha :: Alpha  
  , enginePolicy :: Policy
  } deriving (Eq, Show)

-- | Create new engine with default settings
newEngine :: Engine
newEngine = Engine
  { engineHooks = emptyStore
  , engineAlpha = Alpha 0.1  -- Default learning rate
  , enginePolicy = defaultPolicy
  }

-- | Create engine with custom policy
engineWithPolicy :: Policy -> Engine  
engineWithPolicy policy = newEngine { enginePolicy = policy }

-- | Default policy settings
defaultPolicy :: Policy
defaultPolicy = Policy
  { policyMinimizeInput = True
  , policySuccessWeight = 1.0
  , policyMaxCascadeDepth = 3
  }

-- * Store Operations

-- | Create condition key for indexing using normalized form for stable keys
conditionKey :: Condition Context -> T.Text
conditionKey = T.pack . show . normalizeCondition

-- | Create empty hook store
emptyStore :: HookStore
emptyStore = HookStore Map.empty Map.empty

-- | Insert hook into store
insertHook :: Hook -> HookStore -> HookStore
insertHook hook store = store
  { storeHooks = Map.insert (hookId hook) hook (storeHooks store)
  , storeIndices = 
      let k = conditionKey (hookCondition hook)
      in Map.insertWith (<>) k [hookId hook] (storeIndices store)
  }

-- | Lookup hook by ID
lookupHook :: HookId -> HookStore -> Maybe Hook
lookupHook hid store = Map.lookup hid (storeHooks store)

-- | Delete hook from store
deleteHook :: HookId -> HookStore -> HookStore  
deleteHook hid store = store
  { storeHooks = Map.delete hid (storeHooks store)
  -- TODO: Clean up indices
  }

-- | Update existing hook
updateHook :: Hook -> HookStore -> HookStore
updateHook hook store = insertHook hook (deleteHook (hookId hook) store)

-- * Smart Constructors with Validation

-- | Create Alpha with bounds checking (total)
-- Total: exhaustive guard pattern with default case
mkAlpha :: Double -> Maybe Alpha
mkAlpha x 
  | x > 0 && x < 1 = Just (Alpha x)
  | otherwise = Nothing

-- | Create Score with bounds checking (total)  
-- Total: exhaustive guard pattern with default case
mkScore :: Double -> Maybe Score
mkScore x
  | x >= 0 && x <= 1 = Just (Score x)
  | otherwise = Nothing

-- | Create Cost (non-negative, total)
-- Total: exhaustive guard pattern with default case
mkCost :: Double -> Maybe Cost  
mkCost x
  | x >= 0 = Just (Cost x)
  | otherwise = Nothing

-- | Create Specificity (non-negative, total)
-- Total: exhaustive guard pattern with default case
mkSpecificity :: Double -> Maybe Specificity
mkSpecificity x
  | x >= 0 = Just (Specificity x) 
  | otherwise = Nothing
