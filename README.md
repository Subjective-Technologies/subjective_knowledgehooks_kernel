# Knowledge Hook Kernel

A mathematically principled, total, and testable Haskell implementation of the **Algebra of Knowledge Hooks**. This kernel provides a pure functional foundation for knowledge automation systems, modeling hooks as first-class algebraic objects with rigorous denotational semantics.

## Overview

Knowledge Hooks represent automated responses to contextual conditions, forming a complete algebraic system with composition, refinement, and learning operations. This implementation prioritizes mathematical rigor, type safety, and comprehensive property-based testing.

### Key Features

- **Total Functions**: No partial functions, all operations are mathematically complete
- **Pure Kernel**: Referentially transparent core with separate IO interpretation layer  
- **Algebraic Laws**: All operations satisfy formal mathematical properties
- **Property Testing**: Comprehensive QuickCheck tests verify algebraic laws
- **Type Safety**: Invariants encoded in the type system prevent invalid states
- **Condition Normalization**: Robust structural equality for complex logical expressions

## Mathematical Foundation

### Denotational Semantics

The kernel provides precise mathematical meaning for each construct:

```haskell
⟦Condition⟧ :: Context -> Bool           -- Predicates over system state
⟦Action⟧    :: Context -> OutcomePlan     -- Pure plans of operations  
⟦Hook⟧      :: Context -> Maybe OutcomePlan -- Conditional automation
```

### Core Algebraic Laws

All laws are verified through comprehensive property tests:

1. **Minimization Law**: Hook prioritization minimizes expected user input cost
2. **Correction Law**: Negative reinforcement learning with bounded score updates  
3. **Equivalence Law**: Semantic equivalence implies behavioral equivalence
4. **Refinement Partial Order**: Hooks form a preorder under specificity refinement
5. **Composition Laws**: Associative composition with identity elements
6. **Boolean Algebra**: Conditions form a complete Boolean algebra
7. **Cascading Termination**: Bounded cascading prevents infinite loops

### Negative Reinforcement Learning

Success scores are updated using the bounded formula:
```
success' = (1 - α) * success + α * (1 - correctionFlag)
```
Where α ∈ (0,1) and the result is guaranteed ∈ [0,1].

## Architecture

### Module Organization

```
src/KnowledgeHook/
├── Types.hs          -- Core algebraic types and data structures
├── Semantics.hs      -- Denotational semantics and condition normalization
├── Algebra.hs        -- Algebraic operations (composition, learning, etc.)
├── Core.hs           -- Public API and engine interface
├── Interpret.hs      -- Pure interpreters for actions
└── Examples.hs       -- Demonstration scenarios and workflows
```

### Law-to-Module Mapping

| Algebraic Law | Primary Module | Supporting Modules |
|---------------|----------------|-------------------|
| Minimization Law | `Algebra.hs` (`prioritize`) | `Core.hs` (policy) |
| Correction Law | `Algebra.hs` (`negativeRLUpdate`) | `Types.hs` (bounds) |
| Equivalence Law | `Algebra.hs` (`equivalent`) | `Semantics.hs` (`normalizeCondition`) |
| Refinement Order | `Algebra.hs` (`refine`, `calculateSpecificity`) | `Types.hs` (`Specificity`) |
| Composition Laws | `Algebra.hs` (`composeNested`, `composeFlat`) | `Types.hs` (monoids) |
| Boolean Algebra | `Semantics.hs` (`andP`, `orP`, `notP`) | `Types.hs` (`Condition`) |
| Cascading Termination | `Algebra.hs` (`cascade`) | `Core.hs` (`Policy`) |

## Public API

### Core Engine

```haskell
-- Engine with hooks, learning parameters, and policy
data Engine = Engine 
  { engineHooks :: HookStore
  , engineAlpha :: Alpha      -- Learning rate α ∈ (0,1)
  , enginePolicy :: Policy    -- Selection and execution policy
  }

-- Create and configure engines
newEngine :: Engine
engineWithPolicy :: Policy -> Engine
```

### Hook Operations

```haskell
-- Find hooks matching current context
activate :: HookStore -> Context -> [HookMatch]

-- Select optimal hook using minimization law
prioritize :: Policy -> [HookMatch] -> Maybe HookMatch

-- Execute hook with pure interpretation
execute :: Hook -> Context -> Outcome

-- Learn new hook from behavioral delta
learnDelta :: Alpha -> Snapshot -> Snapshot -> UserInput -> HookStore -> HookStore
```

### Algebraic Operations

```haskell
-- Hook composition
composeNested :: Hook -> Hook -> Hook  -- Sequential execution
composeFlat :: Hook -> Hook -> Hook    -- Parallel/alternative execution

-- Refinement and equivalence
refine :: Hook -> Hook -> Hook         -- Make hook more specific
equivalent :: Hook -> Hook -> Equiv    -- Test semantic equivalence

-- Cascading
cascade :: Policy -> HookStore -> Outcome -> [Hook] -- Trigger follow-on hooks
```

### Condition Construction

```haskell
-- Boolean combinators with smart constructors
andP, orP :: Condition a -> Condition a -> Condition a
notP :: Condition a -> Condition a

-- Context predicates
hasText :: Text -> Condition Context
inApp :: Text -> Condition Context  
timeAfter, timeBefore :: UTCTime -> Condition Context
featureEq :: Feature -> FeatureValue -> Condition Context

-- Normalization for robust equality
normalizeCondition :: Condition Context -> Condition Context
```

## Building and Testing

### Prerequisites

- GHC 9.2+ (recommended)
- Stack 2.7+ or Cabal 3.6+

### Build Commands

```bash
# Using Stack (recommended)
stack build                    # Build the project
stack test                     # Run property tests  
stack run knowledgehook-demo   # Run demonstration

# Using Cabal
cabal build
cabal test
cabal run knowledgehook-demo
```

### Running Tests

The kernel includes comprehensive property-based tests:

```bash
# Run all property tests
stack test

# Run with verbose output
stack test --test-arguments="--verbose"

# Run specific test suites
stack test --test-arguments="--match 'Minimization Law'"
stack test --test-arguments="--match 'Boolean Algebra'"
```

### Expected Test Output

```
Knowledge Hook Properties
  Minimization Law
    ✓ prioritize chooses lowest cost hook (100 tests)
    ✓ prioritize breaks ties with success score (100 tests)  
    ✓ prioritize is deterministic (100 tests)
  Correction Law (Negative RL)
    ✓ correction decreases success score (100 tests)
    ✓ no correction increases success score (100 tests)
    ✓ success score bounded in [0,1] (100 tests)
    ✓ repeated corrections approach 0 (100 tests)
    ✓ repeated non-corrections approach 1 (100 tests)
  [... all other laws ...]

Finished in 0.3247 seconds  
21 examples, 0 failures
```

## Examples

### Basic Usage

```haskell
import KnowledgeHook.Core

-- Create engine with default policy
engine = newEngine

-- Define context
context = Context features currentTime (Just "email")

-- Find and execute hooks
case prioritize (enginePolicy engine) =<< activate (engineHooks engine) context of
  Just match -> do
    let outcome = execute (matchHook match) context
    -- Handle outcome...
  Nothing -> putStrLn "No applicable hooks"
```

### Learning from User Behavior

```haskell
-- Learn from user actions
let beforeSnapshot = Snapshot oldContext time1 "before"
    afterSnapshot = Snapshot newContext time2 "after"  
    userInput = UserInput [OpenApp "calendar", TypeText "Meeting"] [] time1
    
    learnedEngine = engine 
      { engineHooks = learnDelta (Alpha 0.1) beforeSnapshot afterSnapshot userInput (engineHooks engine)
      }
```

### Hook Composition

```haskell
-- Sequential composition
emailThenLog = composeNested sendEmailHook logActivityHook

-- Parallel composition  
copyAndPaste = composeFlat copyHook pasteHook

-- Check equivalence
case equivalent hook1 hook2 of
  Equivalent -> putStrLn "Hooks are semantically equivalent"
  NotEquivalent -> putStrLn "Hooks differ"
```

## Development

### Code Organization

- **Pure Kernel**: All core logic is pure and total
- **Interpreter Layer**: Separate module for IO interpretation
- **Property Tests**: Comprehensive law verification
- **Type Safety**: Invariants encoded in types

### Contributing

1. **Maintain Totality**: All functions must be total (no partial functions)
2. **Preserve Purity**: No IO in core kernel modules
3. **Add Property Tests**: New laws require QuickCheck properties
4. **Update Documentation**: Include denotational semantics for new operations
5. **Follow Type Design**: Use smart constructors and bounded types

### Design Principles

1. **Mathematical Rigor**: All operations have formal semantics
2. **Type Safety**: Invalid states are unrepresentable  
3. **Totality**: No runtime failures from partial functions
4. **Compositionality**: Operations combine predictably
5. **Testability**: All laws are property-tested

## References

- **Knowledge Hook Algebra**: Core mathematical framework
- **Denotational Semantics**: Scott & Strachey approach
- **Boolean Algebras**: Birkhoff & von Neumann
- **Property-Based Testing**: QuickCheck methodology
- **Negative Reinforcement Learning**: Bounded update formulas

## License

BSD-3-Clause - See LICENSE file for details.

---

*A mathematically principled approach to knowledge automation.*
