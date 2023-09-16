{-# LANGUAGE LambdaCase #-}

module Check where

import Proof
import Prop (Prop (..))
import Rule (Rule (..))

data Checked = ValidProp Prop | AsumpDerv Prop Prop deriving (Eq)

type CheckedSteps = [Checked]

data Checker = Checker {premises, assumptions :: [Prop], checkedSteps :: CheckedSteps}

ckFindProp :: CheckedSteps -> StepRef -> (Prop -> Bool) -> Bool
ckFindProp ck (StepRef i) cond = (i < length ck) && (case ck !! i of ValidProp p -> cond p; _ -> False)

ckFindDerv :: CheckedSteps -> StepRef -> (Prop -> Prop -> Bool) -> Bool
ckFindDerv ck (StepRef i) cond = (i < length ck) && (case ck !! i of AsumpDerv p q -> cond p q; _ -> False)

ckIndex :: CheckedSteps -> StepRef -> Maybe Checked
ckIndex ck (StepRef i) = if i < length ck then Just (ck !! i) else Nothing

checkRule :: Checker -> Prop -> Rule -> [StepRef] -> Bool
-- use a premise
checkRule checker p Premise [] = p `elem` premises checker
-- use an assumption
checkRule checker p Assumption [] = p `elem` assumptions checker
-- conjunction introduction
checkRule checker (p `And` q) ConjI [i, j] =
  find i (== p) && find j (== q)
  where
    find = ckFindProp $ checkedSteps checker
-- conjunction elimination
checkRule checker p ConjE [i] =
  ckFindProp (checkedSteps checker) i (\case p' `And` q' -> p == p' || p == q'; _ -> False)
-- disjunction introduction
checkRule checker (p `Or` q) DisjI [i] =
  ckFindProp (checkedSteps checker) i (\p' -> p == p' || q == p')
-- disjunction elimination
checkRule checker x DisjE [i, j, k] =
  case ckIndex steps i of
    Just (ValidProp (p `Or` q)) ->
      ckFindDerv steps j (\asump derived -> asump == p && derived == x)
        && ckFindDerv steps k (\asump derived -> asump == q && derived == x)
    _ -> False
  where
    steps = checkedSteps checker
-- implication introduction
checkRule checker (p `Impl` q) ImplI [i] =
  ckFindDerv (checkedSteps checker) i (\asump derived -> asump == p && derived == q)
-- implication elimination
checkRule checker q ImplE [i, j] =
  case ckIndex steps i of
    Just (ValidProp p) -> ckFindProp steps j (\pq -> pq == p `Impl` q)
    _ -> False
  where
    steps = checkedSteps checker
-- negation introduction
checkRule checker (Not p) NegI [i] =
  ckFindDerv (checkedSteps checker) i (\asump derived -> asump == p && derived == Bottom)
-- negation elimination
checkRule checker Bottom NegE [i, j] =
  case ckIndex steps i of
    Just (ValidProp p) -> ckFindProp steps j (== Not p)
    _ -> False
  where
    steps = checkedSteps checker
-- bottom elimination
checkRule checker _ BotE [i] =
  ckFindProp (checkedSteps checker) i (== Bottom)
-- double negation introduction
checkRule checker (Not (Not p)) NegNegI [i] =
  ckFindProp (checkedSteps checker) i (== p)
-- double negation elimination
checkRule checker p NegNegI [i] =
  ckFindProp (checkedSteps checker) i (== Not (Not p))
-- not a valid application of existing rules
checkRule _ _ _ _ = False