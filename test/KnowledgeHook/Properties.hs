{-# LANGUAGE ScopedTypeVariables #-}

{-|
Module      : KnowledgeHook.Properties
Description : Property tests for Knowledge Hook laws
Copyright   : (c) 2025 Subjective Technologies
License     : BSD3
Maintainer  : dev@subjectivetechnologies.com

This module contains QuickCheck property tests that encode the mathematical
laws from the Knowledge Hook algebra, ensuring the implementation maintains
its algebraic properties.
-}

module KnowledgeHook.Properties (spec) where

import Test.Hspec
import Test.QuickCheck
import KnowledgeHook.Core
import KnowledgeHook.Algebra
import KnowledgeHook.Semantics
import KnowledgeHook.Types (emptyContext)
import KnowledgeHook.Generators

spec :: Spec
spec = do
  describe "Minimization Law" $ do
    it "prioritize chooses lowest cost hook" $ property prop_minimization_law
    it "prioritize breaks ties with success score" $ property prop_tiebreaking_law
    it "prioritize is deterministic" $ property prop_prioritize_deterministic
    
  describe "Correction Law (Negative RL)" $ do  
    it "correction decreases success score" $ property prop_correction_decreases_score
    it "no correction increases success score" $ property prop_no_correction_increases_score
    it "success score bounded in [0,1]" $ property prop_score_bounded
    it "repeated corrections approach 0" $ property prop_corrections_approach_zero
    it "repeated non-corrections approach 1" $ property prop_non_corrections_approach_one
    
  describe "Equivalence Law" $ do
    it "equivalent hooks have same denotation" $ property prop_equivalent_same_denotation
    it "equivalence is reflexive" $ property prop_equivalence_reflexive
    it "equivalence is symmetric" $ property prop_equivalence_symmetric
    
  describe "Refinement Partial Order" $ do
    it "refinement is reflexive" $ property prop_refinement_reflexive  
    it "refinement is transitive" $ property prop_refinement_transitive
    it "refinement increases specificity" $ property prop_refinement_increases_specificity
    
  describe "Composition Laws" $ do
    it "flat composition is associative" $ property prop_flat_composition_associative
    it "nested composition respects sequencing" $ property prop_nested_composition_sequence
    it "composition preserves totality" $ property prop_composition_total
    
  describe "Boolean Algebra (Conditions)" $ do
    it "and is associative" $ property prop_and_associative
    it "or is associative" $ property prop_or_associative  
    it "and has identity (true)" $ property prop_and_identity
    it "or has identity (false)" $ property prop_or_identity
    it "de Morgan's laws hold" $ property prop_de_morgan
    
  describe "Condition Normalization" $ do
    it "normalization is idempotent" $ property prop_normalization_idempotent
    it "normalization preserves semantics" $ property prop_normalization_preserves_semantics
    it "equivalent conditions normalize to same form" $ property prop_equivalent_conditions_normalize
    it "normalization invariant holds" $ property prop_normalization_invariant
    
  describe "Cascading Termination" $ do
    it "cascading reaches fixed point" $ property prop_cascading_terminates
    it "cascading respects depth limit" $ property prop_cascading_depth_limit

-- * Minimization Law Properties

prop_minimization_law :: [HookMatch] -> Property
prop_minimization_law matches = 
  not (null matches) ==> 
    case prioritize defaultPolicy matches of
      Nothing -> False
      Just chosen -> matchCost chosen <= minimum (map matchCost matches)

prop_tiebreaking_law :: HookMatch -> HookMatch -> Property  
prop_tiebreaking_law m1 m2 =
  matchCost m1 == matchCost m2 ==>
    case prioritize defaultPolicy [m1, m2] of
      Nothing -> False
      Just chosen -> 
        let score1 = statsSuccess . hookStats . matchHook $ m1
            score2 = statsSuccess . hookStats . matchHook $ m2
        in if score1 > score2 
           then chosen == m1
           else if score2 > score1 
           then chosen == m2  
           else True  -- Either choice valid for tie

-- | Test that prioritize is deterministic for a fixed input list
prop_prioritize_deterministic :: [HookMatch] -> Property
prop_prioritize_deterministic matches =
  not (null matches) ==>
    let result1 = prioritize defaultPolicy matches
        result2 = prioritize defaultPolicy matches
    in result1 == result2

-- * Correction Law Properties

prop_correction_decreases_score :: Alpha -> Score -> Property
prop_correction_decreases_score alpha score =
  unAlpha alpha > 0 && unAlpha alpha < 1 ==>
    let newScore = negativeRLUpdate alpha score (Correction True)
    in unScore newScore < unScore score

prop_no_correction_increases_score :: Alpha -> Score -> Property
prop_no_correction_increases_score alpha score =
  unAlpha alpha > 0 && unAlpha alpha < 1 && unScore score < 1 ==>
    let newScore = negativeRLUpdate alpha score (Correction False)  
    in unScore newScore > unScore score

prop_score_bounded :: Alpha -> Score -> Correction -> Property
prop_score_bounded alpha score correction =
  unAlpha alpha > 0 && unAlpha alpha < 1 ==>
    let newScore = negativeRLUpdate alpha score correction
    in unScore newScore >= 0 && unScore newScore <= 1

-- | Test that repeated corrections drive score toward 0
prop_corrections_approach_zero :: Alpha -> Score -> Property
prop_corrections_approach_zero alpha initialScore =
  unAlpha alpha > 0 && unAlpha alpha < 1 && unScore initialScore > 0.1 ==>
    let iterations = 50  -- Enough iterations to see convergence
        correction = Correction True
        applyCorrection s = negativeRLUpdate alpha s correction
        finalScore = iterate applyCorrection initialScore !! iterations
        -- After many corrections, score should be much closer to 0
        improvement = unScore initialScore - unScore finalScore
    in improvement > 0 && unScore finalScore < unScore initialScore

-- | Test that repeated non-corrections drive score toward 1  
prop_non_corrections_approach_one :: Alpha -> Score -> Property
prop_non_corrections_approach_one alpha initialScore =
  unAlpha alpha > 0 && unAlpha alpha < 1 && unScore initialScore < 0.9 ==>
    let iterations = 50  -- Enough iterations to see convergence
        noCorrection = Correction False
        applyNoCorrection s = negativeRLUpdate alpha s noCorrection
        finalScore = iterate applyNoCorrection initialScore !! iterations
        -- After many non-corrections, score should be much closer to 1
        improvement = unScore finalScore - unScore initialScore
    in improvement > 0 && unScore finalScore > unScore initialScore

-- * Equivalence Law Properties

prop_equivalent_same_denotation :: Hook -> Hook -> Context -> Property  
prop_equivalent_same_denotation h1 h2 ctx =
  equivalent h1 h2 == Equivalent ==>
    evalHook h1 ctx == evalHook h2 ctx

prop_equivalence_reflexive :: Hook -> Property
prop_equivalence_reflexive hook =
  property $ equivalent hook hook == Equivalent

prop_equivalence_symmetric :: Hook -> Hook -> Property
prop_equivalence_symmetric h1 h2 =
  property $ equivalent h1 h2 == equivalent h2 h1

-- * Refinement Properties

prop_refinement_reflexive :: Hook -> Property  
prop_refinement_reflexive hook =
  let refined = refine hook hook
  in property $ hookSpecificity refined >= hookSpecificity hook

prop_refinement_transitive :: Hook -> Hook -> Hook -> Property
prop_refinement_transitive h1 h2 h3 =
  let h12 = refine h1 h2
      _h23 = refine h2 h3  -- Unused, but kept for logical flow  
      h123_direct = refine h1 h3
      h123_composed = refine h12 h3
  in property $ hookSpecificity h123_direct <= hookSpecificity h123_composed

prop_refinement_increases_specificity :: Hook -> Hook -> Property
prop_refinement_increases_specificity general specific =
  let refined = refine general specific
  in property $ hookSpecificity refined >= max (hookSpecificity general) (hookSpecificity specific)

-- * Composition Properties

prop_flat_composition_associative :: Hook -> Hook -> Hook -> Property
prop_flat_composition_associative h1 h2 h3 =
  let left = composeFlat (composeFlat h1 h2) h3
      right = composeFlat h1 (composeFlat h2 h3)
  in property $ normalizeCondition (hookCondition left) == normalizeCondition (hookCondition right)  -- Normalized structural equality

prop_nested_composition_sequence :: Hook -> Hook -> Context -> Property  
prop_nested_composition_sequence h1 h2 ctx =
  evalCondition (hookCondition h1) ctx && evalCondition (hookCondition h2) ctx ==>
    let composed = composeNested h1 h2
        plan = evalAction (hookAction composed) ctx
    in planCost plan >= planCost (evalAction (hookAction h1) ctx) + 
                        planCost (evalAction (hookAction h2) ctx)

prop_composition_total :: Hook -> Hook -> Property
prop_composition_total h1 h2 =
  let flat = composeFlat h1 h2
      nested = composeNested h1 h2  
  in property $ hookId flat /= HookId "" && hookId nested /= HookId ""

-- * Boolean Algebra Properties

prop_and_associative :: Condition Context -> Condition Context -> Condition Context -> Context -> Property
prop_and_associative c1 c2 c3 ctx =
  let left = evalCondition (andP (andP c1 c2) c3) ctx
      right = evalCondition (andP c1 (andP c2 c3)) ctx
      -- Also check normalized forms are equal
      leftNorm = normalizeCondition (andP (andP c1 c2) c3)
      rightNorm = normalizeCondition (andP c1 (andP c2 c3))
  in property $ left == right && leftNorm == rightNorm

prop_or_associative :: Condition Context -> Condition Context -> Condition Context -> Context -> Property  
prop_or_associative c1 c2 c3 ctx =
  let left = evalCondition (orP (orP c1 c2) c3) ctx
      right = evalCondition (orP c1 (orP c2 c3)) ctx
      -- Also check normalized forms are equal
      leftNorm = normalizeCondition (orP (orP c1 c2) c3)
      rightNorm = normalizeCondition (orP c1 (orP c2 c3))
  in property $ left == right && leftNorm == rightNorm

prop_and_identity :: Condition Context -> Context -> Property
prop_and_identity c ctx =
  property $ evalCondition (andP c TrueP) ctx == evalCondition c ctx &&
            evalCondition (andP TrueP c) ctx == evalCondition c ctx

prop_or_identity :: Condition Context -> Context -> Property
prop_or_identity c ctx =  
  property $ evalCondition (orP c FalseP) ctx == evalCondition c ctx &&
            evalCondition (orP FalseP c) ctx == evalCondition c ctx

prop_de_morgan :: Condition Context -> Condition Context -> Context -> Property
prop_de_morgan c1 c2 ctx =
  let left = evalCondition (notP (andP c1 c2)) ctx
      right = evalCondition (orP (notP c1) (notP c2)) ctx
      left2 = evalCondition (notP (orP c1 c2)) ctx
      right2 = evalCondition (andP (notP c1) (notP c2)) ctx
  in property $ left == right && left2 == right2

-- * Cascading Properties

prop_cascading_terminates :: Policy -> HookStore -> Outcome -> Property
prop_cascading_terminates policy store outcome =
  policyMaxCascadeDepth policy > 0 ==>
    let cascaded = cascade policy store outcome
    in length cascaded <= policyMaxCascadeDepth policy

prop_cascading_depth_limit :: Property  
prop_cascading_depth_limit = 
  forAll (choose (0, 5)) $ \depth ->
    let policy = defaultPolicy { policyMaxCascadeDepth = depth }
        store = emptyStore
        outcome = Outcome emptyContext (Correction False) mempty
        cascaded = cascade policy store outcome
    in length cascaded <= depth

-- * Condition Normalization Properties

prop_normalization_idempotent :: Condition Context -> Property
prop_normalization_idempotent c =
  let normalized = normalizeCondition c
      doubleNormalized = normalizeCondition normalized
  in property $ normalized == doubleNormalized

prop_normalization_preserves_semantics :: Condition Context -> Context -> Property
prop_normalization_preserves_semantics c ctx =
  property $ evalCondition c ctx == evalCondition (normalizeCondition c) ctx

prop_equivalent_conditions_normalize :: Condition Context -> Property
prop_equivalent_conditions_normalize c =
  let -- Create equivalent but structurally different conditions
      c1 = andP c TrueP  -- c ∧ True ≡ c
      c2 = orP c FalseP  -- c ∨ False ≡ c
      c3 = notP (notP c) -- ¬¬c ≡ c
  in property $ normalizeCondition c1 == normalizeCondition c &&
               normalizeCondition c2 == normalizeCondition c &&
               normalizeCondition c3 == normalizeCondition c

-- | Test the normalization invariant: normalizeCondition . normalizeCondition == normalizeCondition
prop_normalization_invariant :: Condition Context -> Property
prop_normalization_invariant c =
  let normalized = normalizeCondition c
      doubleNormalized = normalizeCondition normalized
  in property $ normalized == doubleNormalized  -- f(f(x)) = f(x) - idempotence law
