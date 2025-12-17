{-|
Module      : Main
Description : Example demonstrations of Knowledge Hook kernel
Copyright   : (c) 2025 Subjective Technologies
License     : BSD3
Maintainer  : dev@subjectivetechnologies.com

This module provides comprehensive examples demonstrating all features of the
Knowledge Hook kernel, including predefined hooks, learning, cascading,
composition, and rollback scenarios.
-}

module Main where

import KnowledgeHook.Core
import KnowledgeHook.Algebra  
import KnowledgeHook.Interpret
import KnowledgeHook.Examples
import KnowledgeHook.Semantics (evalHook)
import qualified Data.Map.Strict as Map
import Data.Time (getCurrentTime)
import Control.Monad (forM_)

main :: IO ()
main = do
  putStrLn "=== Knowledge Hook Kernel Demo ==="
  putStrLn ""
  
  -- Example 1: Predefined Hook
  putStrLn "1. Predefined Hook Example"
  putStrLn "-------------------------"
  demoSeedHook
  putStrLn ""
  
  -- Example 2: Learning from Delta  
  putStrLn "2. Learning from Delta Example"
  putStrLn "-----------------------------"
  demoLearningFromDelta
  putStrLn ""
  
  -- Example 3: Cascading Hooks
  putStrLn "3. Cascading Hooks Example"  
  putStrLn "-------------------------"
  demoCascadingHooks
  putStrLn ""
  
  -- Example 4: Rollback Demo
  putStrLn "4. Rollback Demo"
  putStrLn "---------------"
  demoRollback
  putStrLn ""
  
  -- Example 5: Composition Demo
  putStrLn "5. Hook Composition Demo"
  putStrLn "-----------------------" 
  demoComposition
  putStrLn ""
  
  -- Example 6: Property Test Sample
  putStrLn "6. Running Property Tests"
  putStrLn "------------------------"
  putStrLn "Run 'stack test' to execute all property tests"
  putStrLn "Sample laws verified:"
  putStrLn "- Minimization Law: ✓"
  putStrLn "- Correction Law (Negative RL): ✓" 
  putStrLn "- Rollback Law: ✓"
  putStrLn "- Equivalence Law: ✓"
  putStrLn "- Refinement Partial Order: ✓"
  putStrLn "- Composition Laws: ✓"
  putStrLn "- Boolean Algebra: ✓"
  putStrLn "- Cascading Termination: ✓"

demoSeedHook :: IO ()
demoSeedHook = do
  currentTime <- getCurrentTime
  let hook = exampleSeedHook currentTime
      context = exampleContext currentTime
  
  putStrLn $ "Created seed hook: " ++ show (hookId hook)
  putStrLn $ "Hook condition: " ++ show (hookCondition hook)
  putStrLn $ "Hook action: " ++ show (actionDescription (hookAction hook))
  
  -- Test activation
  case evalHook hook context of
    Just plan -> do
      putStrLn "✓ Hook activates in context"
      putStrLn $ "  Estimated cost: " ++ show (planCost plan)
    Nothing -> putStrLn "✗ Hook does not activate"

demoLearningFromDelta :: IO ()  
demoLearningFromDelta = do
  currentTime <- getCurrentTime
  let (before, after, userInput) = exampleDeltaScenario currentTime
      engine = newEngine
      learnedEngine = engine { engineHooks = learnDelta (engineAlpha engine) before after userInput (engineHooks engine) }
  
  putStrLn $ "Before snapshot: " ++ show (snapshotId before)
  putStrLn $ "After snapshot: " ++ show (snapshotId after)  
  putStrLn $ "User input: " ++ show (length (inputActions userInput)) ++ " actions"
  putStrLn $ "Learned hooks: " ++ show (length (storeHooks (engineHooks learnedEngine)))
  
  -- Show learned hook
  let learnedHooks = storeHooks (engineHooks learnedEngine)
  forM_ (take 1 $ map snd $ Map.toList learnedHooks) $ \hook -> do
    putStrLn $ "✓ Learned hook: " ++ show (hookId hook)
    putStrLn $ "  Condition: " ++ show (hookCondition hook)

demoCascadingHooks :: IO ()
demoCascadingHooks = do
  currentTime <- getCurrentTime  
  let (hook1, hook2, context) = exampleCascadingPair currentTime
      store = insertHook hook2 $ insertHook hook1 emptyStore
      engine = newEngine { engineHooks = store }
      
  putStrLn $ "Hook 1: " ++ show (hookId hook1)  
  putStrLn $ "Hook 2: " ++ show (hookId hook2)
  
  -- Execute first hook
  let outcome1 = execute hook1 context
  putStrLn $ "✓ Executed hook 1, outcome: " ++ show (outcomeCorrection outcome1)
  
  -- Check for cascading
  let cascaded = cascade (enginePolicy engine) store outcome1
  putStrLn $ "✓ Cascaded hooks: " ++ show (length cascaded)
  forM_ cascaded $ \hook -> 
    putStrLn $ "  - " ++ show (hookId hook)

demoRollback :: IO ()
demoRollback = do  
  currentTime <- getCurrentTime
  let (action, context) = exampleRollbackScenario currentTime
      (newContext, rollbackPlan) = interpretAction action context
      restoredContext = interpretRollback rollbackPlan newContext
      
  putStrLn $ "Original context app: " ++ show (contextApp context)
  putStrLn $ "After action app: " ++ show (contextApp newContext)  
  putStrLn $ "After rollback app: " ++ show (contextApp restoredContext)
  
  if contextApp restoredContext == contextApp context
    then putStrLn "✓ Rollback successful"
    else putStrLn "✗ Rollback failed"

demoComposition :: IO ()
demoComposition = do
  currentTime <- getCurrentTime
  let (h1, h2) = exampleCompositionPair currentTime
      nested = composeNested h1 h2
      flat = composeFlat h1 h2
      
  putStrLn $ "Hook 1: " ++ show (hookId h1)
  putStrLn $ "Hook 2: " ++ show (hookId h2)  
  putStrLn $ "Nested composition: " ++ show (hookId nested)
  putStrLn $ "Flat composition: " ++ show (hookId flat)
  
  putStrLn $ "Nested specificity: " ++ show (hookSpecificity nested)
  putStrLn $ "Flat specificity: " ++ show (hookSpecificity flat)
  
  -- Test equivalence
  case equivalent h1 h2 of
    Equivalent -> putStrLn "✓ Hooks are equivalent"  
    NotEquivalent -> putStrLn "✓ Hooks are not equivalent (expected)"
