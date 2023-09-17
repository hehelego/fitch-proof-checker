{-# LANGUAGE LambdaCase #-}

module Check (checkProof) where

import Proof
import Prop

data Checked = ValidProp Prop | AsumpDerv Prop Prop deriving (Eq, Show)

type CheckedSteps = [Checked]

data Checker = Checker
  { premises, assumptions :: [Prop],
    checkedSteps :: CheckedSteps, -- checked steps in current block
    valid :: Bool
  }

initChecker :: Checker
initChecker = Checker {premises = [], assumptions = [], checkedSteps = [], valid = True}

ckFindProp :: CheckedSteps -> StepRef -> (Prop -> Bool) -> Bool
ckFindProp ck i cond = (i < length ck) && (case ck !! i of ValidProp p -> cond p; _ -> False)

ckFindDerv :: CheckedSteps -> StepRef -> (Prop -> Prop -> Bool) -> Bool
ckFindDerv ck i cond = (i < length ck) && (case ck !! i of AsumpDerv p q -> cond p q; _ -> False)

ckIndex :: CheckedSteps -> StepRef -> Maybe Checked
ckIndex ck i = if i < length ck then Just (ck !! i) else Nothing

checkRule :: Checker -> Prop -> Rule -> Bool
-- use a premise
checkRule checker p Premise = p `elem` premises checker
-- use an assumption
checkRule checker p Assumption = p `elem` assumptions checker
-- conjunction introduction
checkRule checker (p `And` q) (ConjI i j) =
  find i (== p) && find j (== q)
  where
    find = ckFindProp $ checkedSteps checker
-- conjunction elimination
checkRule checker p (ConjE i) =
  ckFindProp (checkedSteps checker) i (\case p' `And` q' -> p == p' || p == q'; _ -> False)
-- disjunction introduction
checkRule checker (p `Or` q) (DisjI i) =
  ckFindProp (checkedSteps checker) i (\p' -> p == p' || q == p')
-- disjunction elimination
checkRule checker x (DisjE i j k) =
  case ckIndex steps i of
    Just (ValidProp (p `Or` q)) ->
      ckFindDerv steps j (\asump derived -> asump == p && derived == x)
        && ckFindDerv steps k (\asump derived -> asump == q && derived == x)
    _ -> False
  where
    steps = checkedSteps checker
-- implication introduction
checkRule checker (p `Impl` q) (ImplI i) =
  ckFindDerv (checkedSteps checker) i (\asump derived -> asump == p && derived == q)
-- implication elimination
checkRule checker q (ImplE i j) =
  case ckIndex steps i of
    Just (ValidProp p) -> ckFindProp steps j (\pq -> pq == p `Impl` q)
    _ -> False
  where
    steps = checkedSteps checker
-- negation introduction
checkRule checker (Not p) (NegI i) =
  ckFindDerv (checkedSteps checker) i (\asump derived -> asump == p && derived == Bottom)
-- negation elimination
checkRule checker Bottom (NegE i j) =
  case ckIndex steps i of
    Just (ValidProp p) -> ckFindProp steps j (== Not p)
    _ -> False
  where
    steps = checkedSteps checker
-- bottom elimination
checkRule checker _ (BotE i) =
  ckFindProp (checkedSteps checker) i (== Bottom)
-- double negation introduction
checkRule checker (Not (Not p)) (NegNegI i) =
  ckFindProp (checkedSteps checker) i (== p)
-- double negation elimination
checkRule checker p (NegNegE i) =
  ckFindProp (checkedSteps checker) i (== Not (Not p))
-- not a valid application of existing rules
checkRule _ _ _ = False

checkProof :: Proof -> Bool
checkProof (Proof premises steps) = valid $ runChecker initChecker {premises = premises} steps

runChecker :: Checker -> [Step] -> Checker
runChecker = foldl stepChecker

stepChecker :: Checker -> Step -> Checker
stepChecker
  checker@Checker
    { premises = _premises,
      assumptions = _assumptions,
      checkedSteps = _checkedSteps
    }
  step =
    if valid checker
      then case step of
        MakeAssumption asump concl subSteps ->
          let checkSub = runChecker checker {assumptions = asump : _assumptions, checkedSteps = []} subSteps
           in updChecker checker (valid checkSub) (AsumpDerv asump concl)
        ApplyRule prop rule -> let v = checkRule checker prop rule in updChecker checker v (ValidProp prop)
      else checker

updChecker :: Checker -> Bool -> Checked -> Checker
updChecker checker True step = checker {checkedSteps = checkedSteps checker ++ [step]}
updChecker checker False step = checker {valid = False}
