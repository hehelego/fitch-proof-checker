{-# LANGUAGE LambdaCase #-}

module Check (checkProof) where

import Control.Monad.Trans.Maybe
import Control.Monad.Writer
import Data.List (uncons)
import qualified Data.Map as Map
import Proof
import Prop

data Knowledge
  = ValidProp Prop -- this proposition follows from the premises
  | ValidDerv Prop Prop -- this derivation is valid given the premises
  deriving (Eq)

instance Show Knowledge where
  show (ValidProp p) = "Checked " ++ show p
  show (ValidDerv p q) = "Checked " ++ show p ++ " entails " ++ show q

-- knowledge base
type KB = Map.Map StepRef Knowledge

data Checker = Checker
  { premises :: [Prop], -- the premises
    assumptions :: [(Prop, Int)], -- currently visible assumptions: (assumed proposition, line number)
    lineNum :: Int, -- current line, count `AddPremise`, `Assume`, and `ApplyRule`.
    lastProp :: Maybe Prop, -- last proposition line `AddPremise`, `Assume`, and `ApplyRule`.
    kb :: KB -- checked steps in current state
  }
  deriving (Show)

initChecker :: Checker
initChecker =
  Checker
    { premises = [],
      assumptions = [],
      lineNum = 1,
      lastProp = Nothing,
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

type WithLogMayFail = MaybeT (Writer [String])

checkProof :: Proof -> WithLogMayFail Checker
checkProof (Proof steps) = foldl (\ck step -> do ck' <- ck; stepChecker ck' step) (return initChecker) steps

logStep :: Checker -> Step -> WithLogMayFail ()
logStep checker step = tell [replicate m ' ' ++ "Line " ++ show n ++ ": " ++ show step]
  where
    m = 8 * length (assumptions checker)
    n = lineNum checker

logStep' :: Checker -> StepRef -> Knowledge -> WithLogMayFail ()
logStep' checker blk derv = tell [replicate m ' ' ++ show blk ++ " " ++ show derv]
  where
    m = 8 * length (assumptions checker) - 8
    n = lineNum checker - 1

wrapMaybeT :: (Monad m) => Maybe a -> MaybeT m a
wrapMaybeT = MaybeT . return

stepChecker :: Checker -> Step -> WithLogMayFail Checker
stepChecker checker@Checker {premises = _premises, assumptions = _assumptions, lineNum = _lineNum, lastProp = _lastProp, kb = _kb} step = case step of
  AddPremise p ->
    do
      logStep checker step
      return
        checker
          { premises = p : _premises,
            kb = Map.insert (SingleRef _lineNum) (ValidProp p) _kb
          }
          { lineNum = _lineNum + 1,
            lastProp = Just p
          }
  Assume p ->
    do
      logStep checker step
      return
        checker
          { assumptions = (p, _lineNum) : _assumptions,
            kb = Map.insert (SingleRef _lineNum) (ValidProp p) _kb
          }
          { lineNum = _lineNum + 1,
            lastProp = Just p
          }
  ApplyRule p rule ->
    do
      guard $ checkRule checker p rule
      logStep checker step
      return
        checker
          { kb = Map.insert (SingleRef _lineNum) (ValidProp p) _kb
          }
          { lineNum = _lineNum + 1,
            lastProp = Just p
          }
  EndAssumption ->
    do
      ((assumed, i), assumptions') <- wrapMaybeT $ uncons _assumptions
      derived <- wrapMaybeT _lastProp
      let blk = BlockRef i (_lineNum - 1); derv = ValidDerv assumed derived
      logStep' checker blk derv
      return
        checker
          { assumptions = assumptions',
            kb = Map.insert blk derv _kb
          }
          { lastProp = Nothing
          }
