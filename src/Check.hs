module Check (checkProof, checkProofSyntax) where

import Control.Monad.Except
import Control.Monad.Writer
import Data.List (uncons)
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
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
type KB = [(StepRef, Knowledge)]

data Checker = Checker
  { premises :: [Prop], -- the premises
    assumptions :: [(Prop, Int)], -- currently visible assumptions: (assumed proposition, line number)
    lineNum :: Int, -- current line, count `AddPremise`, `Assume`, `EndAssumption`, and `ApplyRule`.
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
      kb = []
    }

-- conjunction introduction
conjI :: Prop -> Knowledge -> Knowledge -> Bool
conjI (p `And` q) (ValidProp p') (ValidProp q') = p == p' && q == q'
conjI _ _ _ = False

-- conjunction elimination
conjE :: Prop -> Knowledge -> Bool
conjE r (ValidProp (p `And` q)) = r == p || r == q
conjE _ _ = False

-- disjunction introduction
disjI :: Prop -> Knowledge -> Bool
disjI (p `Or` q) (ValidProp r) = p == r || q == r
disjI _ _ = False

-- disjunction elimination
disjE :: Prop -> Knowledge -> Knowledge -> Knowledge -> Bool
disjE x (ValidProp (p `Or` q)) (ValidDerv p' px) (ValidDerv q' qx) = p == p' && q == q' && x == px && x == qx
disjE _ _ _ _ = False

-- implication introduction
implI :: Prop -> Knowledge -> Bool
implI (p `Impl` q) (ValidDerv p' q') = p == p' && q == q'
implI _ _ = False

-- implication elimination
implE :: Prop -> Knowledge -> Knowledge -> Bool
implE q (ValidProp p) (ValidProp (p' `Impl` q')) = p == p' && q == q'
implE _ _ _ = False

-- negation introduction
negI :: Prop -> Knowledge -> Bool
negI (Not p) (ValidDerv p' Bottom) = p == p'
negI _ _ = False

-- bottom introduction
botI :: Prop -> Knowledge -> Knowledge -> Bool
botI Bottom (ValidProp p) (ValidProp (Not p')) = p == p'
botI _ _ _ = False

-- bottom elimination
botE _ (ValidProp Bottom) = True
botE _ _ = False

-- double negation introduction
negnegI :: Prop -> Knowledge -> Bool
negnegI (Not (Not p)) (ValidProp p') = p == p'
negnegI _ _ = False

-- double negation elimination
negnegE :: Prop -> Knowledge -> Bool
negnegE p (ValidProp (Not (Not p'))) = p == p'
negnegE _ _ = False

type KBFinder = StepRef -> Maybe Knowledge

checkRule :: KBFinder -> Prop -> Rule -> Bool
checkRule kb p rule = orFalse $ case rule of
  ConjI i j -> conjI p <$> kb i <*> kb j
  ConjE i -> conjE p <$> kb i
  DisjI i -> disjI p <$> kb i
  DisjE i j k -> disjE p <$> kb i <*> kb j <*> kb k
  ImplI i -> implI p <$> kb i
  ImplE i j -> implE p <$> kb i <*> kb j
  NegI i -> negI p <$> kb i
  BotI i j -> botI p <$> kb i <*> kb j
  BotE i -> botE p <$> kb i
  NegNegI i -> negnegI p <$> kb i
  NegNegE i -> negnegE p <$> kb i

type Err = ()

type WithLogMayFail = ExceptT Err (Writer [String])

-- Check proof validity: whetheer every rule application is correct
checkProof :: Proof -> WithLogMayFail Checker
checkProof (Proof steps) = foldM stepChecker initChecker steps

-- Check proof syntax: proof structure
--
-- Proof -> Premises Steps
-- Premises -> epsilon | AddPremise Premises
-- Steps -> epsilon | Assume Steps EndAssumption | ApplyRule
checkProofSyntax :: Proof -> Bool
checkProofSyntax (Proof steps) = premiseFirst && assumptionMatch
  where
    premiseFirst = not $ any isPrem $ dropWhile isPrem steps
    assumptionMatch = (0, 0) == foldl matchIter (0, 0) steps
    matchIter (m, e) (Assume _) = (m, e + 1)
    matchIter (m, e) EndAssumption = (min m (e - 1), e - 1)
    matchIter me _ = me
    isPrem (AddPremise _) = True
    isPrem _ = False

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

stepChecker :: Checker -> Step -> WithLogMayFail Checker
stepChecker checker@Checker {premises = _premises, assumptions = _assumptions, lineNum = _lineNum, lastProp = _lastProp, kb = _kb} step = case step of
  AddPremise p ->
    do
      logStep checker step
      return
        checker
          { premises = p : _premises,
            kb = (SingleRef _lineNum, ValidProp p) : _kb
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
            kb = (SingleRef _lineNum, ValidProp p) : _kb
          }
          { lineNum = _lineNum + 1,
            lastProp = Just p
          }
  ApplyRule p rule ->
    do
      let kbfind i = lookup i (kb checker)
      guard $ checkRule kbfind p rule
      logStep checker step
      return
        checker
          { kb = (SingleRef _lineNum, ValidProp p) : _kb
          }
          { lineNum = _lineNum + 1,
            lastProp = Just p
          }
  EndAssumption ->
    do
      ((assumed, i), assumptions') <- maybeToExcept $ uncons _assumptions
      derived <- maybeToExcept _lastProp
      let blk = BlockRef i _lineNum; derv = ValidDerv assumed derived
      logStep' checker blk derv
      return
        checker
          { assumptions = assumptions',
            kb = (blk, derv) : _kb
          }
          { lineNum = _lineNum + 1,
            lastProp = Nothing
          }

maybeToExcept :: (Monad m) => Maybe a -> ExceptT () m a
maybeToExcept (Just x) = ExceptT . pure $ Right x
maybeToExcept Nothing = ExceptT . pure $ Left ()

orFalse :: Maybe Bool -> Bool
orFalse (Just True) = True
orFalse _ = False
