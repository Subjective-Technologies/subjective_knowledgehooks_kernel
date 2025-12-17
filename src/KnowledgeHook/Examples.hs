{-|
Module      : KnowledgeHook.Examples
Description : Example Knowledge Hooks and scenarios  
Copyright   : (c) 2025 Subjective Technologies
License     : BSD3
Maintainer  : dev@subjectivetechnologies.com

This module provides concrete examples of Knowledge Hooks and usage scenarios
to demonstrate the kernel's capabilities and serve as templates for real-world
applications.
-}

module KnowledgeHook.Examples
  ( -- * Example Hooks
    exampleSeedHook
  , exampleLearnedHook
    -- * Example Scenarios
  , exampleContext
  , exampleDeltaScenario
  , exampleCascadingPair
  , exampleRollbackScenario
  , exampleCompositionPair
    -- * Utilities
  , createSampleEngine
  , demonstrateWorkflow
  ) where

import KnowledgeHook.Core
import KnowledgeHook.Algebra
import Data.Time (UTCTime, addUTCTime)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T

-- * Example Hooks

-- | A predefined seed hook for email automation
exampleSeedHook :: UTCTime -> Hook
exampleSeedHook currentTime = Hook
  { hookId = HookId "seed_email_reply"
  , hookCondition = andP (inApp "email") (hasText "meeting")
  , hookAction = Action
      { actionPlan = OutcomePlan
          [ ClickElement "reply_button"
          , TypeText "I'll be there!"
          , ClickElement "send_button"  
          ]
          (RollbackPlan
            [ ClickElement "undo_send"
            , ClickElement "delete_draft"
            ]
            (exampleContext currentTime)
          )
          (Cost 3.0)
      , actionDescription = "Quick meeting reply"
      }
  , hookStats = Stats (Score 0.8) 15 2  -- 80% success, 15 uses, 2 corrections
  , hookMeta = Metadata currentTime currentTime "predefined" ["email", "meeting"]
  , hookSpecificity = Specificity 0.7  -- Fairly specific
  }

-- | An example learned hook
exampleLearnedHook :: UTCTime -> Hook  
exampleLearnedHook currentTime = Hook
  { hookId = HookId "learned_calendar_create"
  , hookCondition = andP (inApp "calendar") (hasText "appointment")
  , hookAction = Action
      { actionPlan = OutcomePlan
          [ ClickElement "new_event"
          , TypeText "Doctor Appointment"
          , ClickElement "save"
          ]
          (RollbackPlan
            [ ClickElement "delete_event"
            ]
            (exampleContext currentTime)
          )
          (Cost 2.5)
      , actionDescription = "Create calendar event"  
      }
  , hookStats = Stats (Score 0.6) 3 1  -- Recently learned, moderate success
  , hookMeta = Metadata currentTime currentTime "learned" ["calendar"]
  , hookSpecificity = Specificity 0.5
  }

-- * Example Scenarios

-- | Example context representing current state
exampleContext :: UTCTime -> Context
exampleContext currentTime = Context
  { contextFeatures = Map.fromList
      [ (Feature "current_text" "text", TextValue "Schedule a meeting for tomorrow")
      , (Feature "window_title" "ui", TextValue "Email - Inbox")
      , (Feature "cursor_position" "ui", NumericValue 42)
      ]
  , contextTime = currentTime
  , contextApp = Just "email"
  }

-- | Example delta learning scenario
exampleDeltaScenario :: UTCTime -> (Snapshot, Snapshot, UserInput)
exampleDeltaScenario currentTime = 
  let beforeCtx = exampleContext currentTime
      afterCtx = beforeCtx 
        { contextApp = Just "calendar"
        , contextFeatures = Map.insert 
            (Feature "new_event" "ui") 
            (TextValue "Meeting with Bob") 
            (contextFeatures beforeCtx)
        }
      before = Snapshot beforeCtx currentTime "snapshot_before"
      after = Snapshot afterCtx (addUTCTime 60 currentTime) "snapshot_after"
      userInput = UserInput
        [ OpenApp "calendar"
        , ClickElement "new_event"  
        , TypeText "Meeting with Bob"
        , ClickElement "save"
        ]
        ["Had to manually open calendar"]
        (addUTCTime 30 currentTime)
  in (before, after, userInput)

-- | Example cascading hook pair
exampleCascadingPair :: UTCTime -> (Hook, Hook, Context)
exampleCascadingPair currentTime =
  let hook1 = Hook
        { hookId = HookId "send_email"
        , hookCondition = hasText "send email"
        , hookAction = Action
            { actionPlan = OutcomePlan
                [ TypeText "Email sent successfully!"
                ]
                (RollbackPlan [] (exampleContext currentTime))
                (Cost 1.0)
            , actionDescription = "Send email"
            }
        , hookStats = Stats (Score 0.9) 10 0
        , hookMeta = Metadata currentTime currentTime "predefined" ["email"]
        , hookSpecificity = Specificity 0.4
        }
      hook2 = Hook  
        { hookId = HookId "log_activity"
        , hookCondition = hasText "Email sent successfully!"
        , hookAction = Action
            { actionPlan = OutcomePlan
                [ OpenApp "logger"
                , TypeText "Email activity logged"
                ]
                (RollbackPlan [] (exampleContext currentTime))
                (Cost 0.5)
            , actionDescription = "Log email activity"
            }
        , hookStats = Stats (Score 0.95) 8 0  
        , hookMeta = Metadata currentTime currentTime "predefined" ["logging"]
        , hookSpecificity = Specificity 0.6
        }
      context = Context
        { contextFeatures = Map.fromList
            [(Feature "user_intent" "text", TextValue "send email to client")]
        , contextTime = currentTime
        , contextApp = Just "email"
        }
  in (hook1, hook2, context)

-- | Example rollback scenario  
exampleRollbackScenario :: UTCTime -> (Action, Context)
exampleRollbackScenario currentTime = 
  let action = Action
        { actionPlan = OutcomePlan
            [ OpenApp "notepad"
            , TypeText "This is a test note"
            , ClickElement "save_as"
            ]
            (RollbackPlan
              [ ClickElement "delete_file"
              , OpenApp "previous_app"
              ]
              (exampleContext currentTime)  -- Original context
            )
            (Cost 2.0)
        , actionDescription = "Create and save note"
        }
      context = exampleContext currentTime
  in (action, context)

-- | Example composition pair
exampleCompositionPair :: UTCTime -> (Hook, Hook)
exampleCompositionPair currentTime =
  let hook1 = Hook
        { hookId = HookId "copy_text"
        , hookCondition = hasText "copy this"
        , hookAction = Action
            { actionPlan = OutcomePlan
                [ SendKeys "Ctrl+C"
                ]
                (RollbackPlan [] (exampleContext currentTime))
                (Cost 0.1)
            , actionDescription = "Copy selected text"
            }
        , hookStats = Stats (Score 0.95) 100 1
        , hookMeta = Metadata currentTime currentTime "predefined" ["clipboard"]
        , hookSpecificity = Specificity 0.3
        }
      hook2 = Hook
        { hookId = HookId "paste_text"  
        , hookCondition = hasText "paste here"
        , hookAction = Action
            { actionPlan = OutcomePlan
                [ SendKeys "Ctrl+V"
                ]
                (RollbackPlan 
                  [ SendKeys "Ctrl+Z"  -- Undo paste
                  ] 
                  (exampleContext currentTime)
                )
                (Cost 0.1)
            , actionDescription = "Paste clipboard content"
            }
        , hookStats = Stats (Score 0.92) 80 3
        , hookMeta = Metadata currentTime currentTime "predefined" ["clipboard"]
        , hookSpecificity = Specificity 0.3
        }
  in (hook1, hook2)

-- * Utilities

-- | Create a sample engine with predefined hooks
createSampleEngine :: UTCTime -> Engine
createSampleEngine currentTime = 
  let seedHook = exampleSeedHook currentTime
      learnedHook = exampleLearnedHook currentTime
      store = insertHook learnedHook $ insertHook seedHook emptyStore
  in newEngine { engineHooks = store }

-- | Demonstrate a complete workflow
demonstrateWorkflow :: UTCTime -> IO ()
demonstrateWorkflow currentTime = do
  let engine = createSampleEngine currentTime
      context = exampleContext currentTime
      matches = activate (engineHooks engine) context
  
  putStrLn $ "Found " ++ show (length matches) ++ " matching hooks"
  
  case prioritize (enginePolicy engine) matches of
    Nothing -> putStrLn "No hooks to execute"
    Just match -> do
      let hook = matchHook match
          outcome = execute hook context
      putStrLn $ "Executed hook: " ++ show (hookId hook)
      putStrLn $ "Outcome correction: " ++ show (outcomeCorrection outcome)
      
      -- Update statistics based on outcome
      let updatedHook = updateStats (Alpha 0.1) hook (outcomeCorrection outcome)
      putStrLn $ "Updated success score: " ++ show (statsSuccess (hookStats updatedHook))
