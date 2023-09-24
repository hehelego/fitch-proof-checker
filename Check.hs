{-# LANGUAGE LambdaCase #-}

module Check (checkProof) where

import qualified Data.Map as Map
import Data.Maybe (fromJust, isJust, isNothing)
import qualified Data.Set as Set
import Debug.Trace
import Proof
import Prop
import qualified StackMultiSet as SMS

data Knowledge
  = ValidProp Prop -- this proposition follows from the premises
  | ValidDerv Prop Prop -- this derivation is valid given the premises
  deriving (Eq, Show)

-- knowledge base
type KB = Map.Map StepRef Knowledge

data Checker = Checker
  { premises :: Set.Set Prop, -- the premises
    assumptions :: SMS.StackMultiSet (Prop, Int), -- currently visible assumptions
    stepCnt :: Int, -- current step, count `AddPremise`, `Assume`, and `ApplyRule`.
    lastProp :: Either String Prop, -- last proposition in `AddPremise`, `Assume`, and `ApplyRule`.
    kb :: KB -- checked steps in current state
  }
  deriving (Show)

initChecker :: Checker
initChecker =
  Checker
    { premises = Set.empty,
      assumptions = SMS.empty,
      stepCnt = 1,
      lastProp = Left "last step not found",
      kb = Map.empty
    }

kbFindProp :: KB -> StepRef -> (Prop -> Bool) -> Bool
kbFindProp kb i cond = maybe False (\case ValidProp p -> cond p; _ -> False) (Map.lookup i kb)

kbFindDerv :: KB -> StepRef -> (Prop -> Prop -> Bool) -> Bool
kbFindDerv kb i cond = maybe False (\case ValidDerv p q -> cond p q; _ -> False) (Map.lookup i kb)

kbIndex :: KB -> StepRef -> Maybe Knowledge
kbIndex = flip Map.lookup

checkRule :: Checker -> Prop -> Rule -> Bool
-- conjunction introduction
checkRule checker (p `And` q) (ConjI i j) =
  findProp i (== p) && findProp j (== q)
  where
    findProp = kbFindProp $ kb checker
-- conjunction elimination
checkRule checker x (ConjE i) =
  kbFindProp (kb checker) i (\case p `And` q -> x == p || x == q; _ -> False)
-- disjunction introduction
checkRule checker (p `Or` q) (DisjI i) =
  kbFindProp (kb checker) i (\r -> p == r || q == r)
-- disjunction elimination
checkRule checker x (DisjE i j k) =
  trace
    (show $ kb checker)
    findProp
    i
    ( \case
        p `Or` q ->
          findDerv j (\assumed derived -> assumed == p && derived == x)
            && findDerv k (\assumed derived -> assumed == q && derived == x)
        _ -> False
    )
  where
    findProp = kbFindProp $ kb checker
    findDerv = kbFindDerv $ kb checker
-- implication introduction
checkRule checker (p `Impl` q) (ImplI i) =
  kbFindDerv (kb checker) i (\assumed derived -> assumed == p && derived == q)
-- implication elimination
checkRule checker q (ImplE i j) =
  findProp
    i
    (\p -> findProp j (\pq -> pq == p `Impl` q))
  where
    findProp = kbFindProp $ kb checker
-- negation introduction
checkRule checker (Not p) (NegI i) =
  kbFindDerv (kb checker) i (\assumed derived -> assumed == p && derived == Bottom)
-- negation elimination
checkRule checker Bottom (NegE i j) =
  findProp
    i
    (\p -> findProp j (== Not p))
  where
    findProp = kbFindProp $ kb checker
-- bottom elimination
checkRule checker _ (BotE i) =
  kbFindProp (kb checker) i (== Bottom)
-- double negation introduction
checkRule checker (Not (Not p)) (NegNegI i) =
  kbFindProp (kb checker) i (== p)
-- double negation elimination
checkRule checker p (NegNegE i) =
  kbFindProp (kb checker) i (== Not (Not p))
-- not a valid application of existing rules
checkRule _ _ _ = False

checkProof :: Proof -> Either String Checker
checkProof (Proof steps) = runChecker initChecker steps

runChecker :: Checker -> [Step] -> Either String Checker
runChecker checker = foldl (\ck step -> do ck' <- ck; stepChecker ck' step) (Right checker)

stepChecker :: Checker -> Step -> Either String Checker
stepChecker checker@Checker {premises = _premises, assumptions = _assumptions, stepCnt = _stepCnt, lastProp = _lastProp, kb = _kb} =
  \case
    AddPremise p ->
      Right
        checker
          { premises = Set.insert p _premises,
            kb = Map.insert (SingleRef _stepCnt) (ValidProp p) _kb
          }
          { stepCnt = _stepCnt + 1,
            lastProp = Right p
          }
    Assume p ->
      Right
        checker
          { assumptions = SMS.push _assumptions (p, _stepCnt),
            kb = Map.insert (SingleRef _stepCnt) (ValidProp p) _kb
          }
          { stepCnt = _stepCnt + 1,
            lastProp = Right p
          }
    ApplyRule p rule ->
      if checkRule checker p rule
        then
          Right
            checker
              { kb = Map.insert (SingleRef _stepCnt) (ValidProp p) _kb
              }
              { stepCnt = _stepCnt + 1,
                lastProp = Right p
              }
        else Left $ "On line " ++ show _stepCnt ++ ": Fail on " ++ show rule
    EndAssumption ->
      do
        (assumptions', (assumed, i)) <- SMS.pop _assumptions
        derived <- _lastProp
        return
          checker
            { assumptions = assumptions',
              kb = Map.insert (BlockRef i (_stepCnt - 1)) (ValidDerv assumed derived) _kb
            }
            { lastProp = Left "last step not found"
            }
