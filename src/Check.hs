module Check (checkProof, checkProofSyntax) where

import Control.Monad.Except
import Control.Monad.Writer
import Data.List (uncons)
import Data.Maybe (fromMaybe, isJust)
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

type WithLogMayFail = ExceptT Err (Writer String)

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

logLine :: Checker -> WithLogMayFail ()
logLine checker = label >> indent >> tell " "
  where
    indent = tell $ replicate level '\t'
    level = length (assumptions checker)
    label = tell "L" >> tell (show line)
    line = lineNum checker

stepChecker :: Checker -> Step -> WithLogMayFail Checker
stepChecker checker@Checker {premises = premises, assumptions = assumptions, lineNum = lineNum, lastProp = lastProp, kb = kb} step = do
  logLine checker
  tell (show step)
  tell "\n"

  -- check validity
  guard $ case step of
    ApplyRule p rule -> let kbfind i = lookup i kb in checkRule kbfind p rule
    EndAssumption -> not (null assumptions) && isJust lastProp
    _ -> True

  -- update knowledgebase
  let checker' = case step of
        AddPremise p -> checker {premises = p : premises, kb = (SingleRef lineNum, ValidProp p) : kb}
        Assume p -> checker {assumptions = (p, lineNum) : assumptions, kb = (SingleRef lineNum, ValidProp p) : kb}
        ApplyRule p rule -> checker {kb = (SingleRef lineNum, ValidProp p) : kb}
        EndAssumption ->
          let ((assumed, i) : assumptions') = assumptions
              Just derived = lastProp
              block = BlockRef i lineNum
              derv = ValidDerv assumed derived
              kb' = filter (not . inScope block . fst) kb
           in checker {assumptions = assumptions', kb = (block, derv) : kb'}
  -- last checked formula
  let last = case step of
        AddPremise p -> Just p
        Assume p -> Just p
        ApplyRule p _ -> Just p
        EndAssumption -> Nothing
  return checker' {lastProp = last, lineNum = lineNum + 1}

maybeToExcept :: (Monad m) => Maybe a -> ExceptT () m a
maybeToExcept (Just x) = ExceptT . pure $ Right x
maybeToExcept Nothing = ExceptT . pure $ Left ()

orFalse :: Maybe Bool -> Bool
orFalse (Just True) = True
orFalse _ = False
