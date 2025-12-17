# Knowledge Hook Kernel - Execution Guide

## Current Status

This is a **pure functional kernel/library**, not a standalone service. It provides the mathematical foundation for knowledge automation systems.

## How to Execute

### Prerequisites

1. **Install Stack** (recommended):
   ```bash
   # Windows (using Chocolatey)
   choco install haskell-stack
   
   # Or download from: https://docs.haskellstack.org/en/stable/install_and_upgrade/
   ```

2. **Or install GHC + Cabal**:
   ```bash
   # Windows: https://www.haskell.org/downloads/
   ```

### Running the Demo

```bash
# Build the project
stack build

# Run the demonstration executable
stack run knowledgehook-demo
```

This will execute examples showing:
- Predefined hooks
- Learning from user behavior
- Cascading hooks
- Rollback scenarios
- Hook composition

### Running Tests

```bash
# Run all property tests (QuickCheck)
stack test

# Run with verbose output
stack test --test-arguments="--verbose"

# Run specific test suites
stack test --test-arguments="--match 'Minimization Law'"
stack test --test-arguments="--match 'Boolean Algebra'"
```

**Test Coverage:**
- ✅ 24 property tests covering all algebraic laws
- ✅ Minimization Law (3 tests)
- ✅ Correction Law / Negative RL (5 tests)
- ✅ Equivalence Law (3 tests)
- ✅ Refinement Partial Order (3 tests)
- ✅ Composition Laws (3 tests)
- ✅ Boolean Algebra (5 tests)
- ✅ Condition Normalization (4 tests)
- ✅ Cascading Termination (2 tests)

## Using as a Library

The kernel can be integrated into your own Haskell applications:

```haskell
import KnowledgeHook.Core

-- Create an engine
let engine = newEngine

-- Define context
let context = Context features currentTime (Just "email")

-- Activate matching hooks
let matches = activate (engineHooks engine) context

-- Prioritize and execute
case prioritize (enginePolicy engine) matches of
  Just match -> 
    let outcome = execute (matchHook match) context
    in -- Handle outcome...
  Nothing -> -- No matching hooks
```

## Making it a Service

To turn this into a web service, you would need to add:

### Option 1: Using Servant (Type-Safe APIs)

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
import Servant
import KnowledgeHook.Core

type API = 
  "hooks" :> Get '[JSON] [Hook]
  :<|> "hooks" :> ReqBody '[JSON] Context :> Post '[JSON] [HookMatch]
  :<|> "execute" :> ReqBody '[JSON] HookId :> Post '[JSON] Outcome

server :: Server API
server = getHooks :<|> activateHooks :<|> executeHook
  where
    getHooks = -- Return all hooks
    activateHooks ctx = -- Activate hooks for context
    executeHook hid = -- Execute specific hook
```

### Option 2: Using Warp (Simple HTTP Server)

```haskell
import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import qualified Data.Aeson as JSON
import KnowledgeHook.Core

app :: Application
app req respond = case pathInfo req of
  ["hooks", "activate"] -> do
    body <- strictRequestBody req
    let ctx = JSON.decode body :: Maybe Context
    case ctx of
      Just context -> do
        let matches = activate engineHooks context
        respond $ responseLBS status200 [("Content-Type", "application/json")] 
                  (JSON.encode matches)
      Nothing -> respond $ responseLBS status400 [] "Invalid context"
  _ -> respond $ responseLBS status404 [] "Not found"

main = run 8080 app
```

### Option 3: Using Scotty (REST-like)

```haskell
import Web.Scotty
import KnowledgeHook.Core

main = scotty 3000 $ do
  get "/hooks" $ do
    json $ storeHooks (engineHooks myEngine)
  
  post "/hooks/activate" $ do
    ctx <- jsonData :: ActionM Context
    let matches = activate (engineHooks myEngine) ctx
    json matches
  
  post "/hooks/:id/execute" $ do
    hid <- param "id"
    ctx <- jsonData :: ActionM Context
    case lookupHook (HookId hid) (engineHooks myEngine) of
      Just hook -> json $ execute hook ctx
      Nothing -> status status404
```

## What's Missing for a Service

1. **State Management**: Currently uses pure `HookStore`. Need:
   - Database persistence (PostgreSQL, SQLite, etc.)
   - In-memory state with TVar/MVar for concurrent access
   - Redis for distributed state

2. **IO Integration**: The kernel is pure. Need:
   - Connect `Action` operations to real UI automation (Selenium, etc.)
   - Connect `Context` to real system state
   - Connect learning to persistent storage

3. **API Layer**: REST/GraphQL endpoints for:
   - CRUD operations on hooks
   - Context submission
   - Hook activation and execution
   - Learning endpoints

4. **Configuration**: Service configuration for:
   - Port/host binding
   - Database connection
   - Logging
   - CORS/authentication

## Current Capabilities

✅ **Pure Mathematical Operations**
- Hook activation, prioritization, execution
- Learning from behavioral deltas
- Composition, refinement, equivalence
- Property-based testing

✅ **Demo Executable**
- Shows all features working
- Runs in command-line

✅ **Comprehensive Tests**
- 24 property tests
- QuickCheck generators
- All algebraic laws verified

❌ **NOT Included**
- Web server
- REST API
- Database persistence
- Real-world IO integration
- Service deployment

## Next Steps

1. **If you want to use it as-is**: Run the demo and tests to see it work
2. **If you want a service**: Add one of the web frameworks above + persistence
3. **If you want to extend it**: The pure kernel is well-designed for extension

