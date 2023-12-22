module Check (checkProof, checkProofSyntax) where

import Control.Monad.Except
import Control.Monad.Writer
import Data.List (find, uncons)
import Data.Maybe (fromMaybe, isJust)
import Debug.Trace (trace)
import Proof
import Prop
import Rules

-- a piece of knowledge is a proved proposition, possibly within certain context
type Knowledge = Prop

-- Line number
type LineNumber = Int

-- referring to a context block. If CTX-A < CTX-B, then CTX-B should be a sub-context of CTX-A
type Context = Int

-- knowledge base
type KB = [(Context, LineNumber, Knowledge)]

data Checker = Checker
  { prems :: [Prop], -- the premises
    asumps :: [Prop], -- currently visible assumptions
    ctx :: Context, -- context level, +1 on Assumption introduction, -1 on Assumption elimination
    ln :: LineNumber, -- line number
    kb :: KB -- proved proposition in scope knowledge
  }
  deriving (Show)

initChecker :: Checker
initChecker = Checker {prems = [], asumps = [], ctx = 0, ln = 1, kb = []}

type Err = String

type WithLogMayFail = ExceptT Err (Writer String)

type KBFinder = StepRef -> WithLogMayFail Knowledge

mkFinder :: KB -> KBFinder
mkFinder kb i = _3rd <$> maybeToExcept findRes errMsg
  where
    _3rd (_, _, x) = x
    findCond (_, l, p) = i == l
    findRes = find findCond kb
    errMsg = "Cannot find " ++ show i ++ " in " ++ show kb

checkRule :: KBFinder -> Prop -> Rule -> WithLogMayFail Bool
checkRule kb p rule = case rule of
  ConjI i j -> conjI <$> kb i <*> kb j <*> pure p
  ConjE i -> conjE <$> kb i <*> pure p
  DisjI i -> disjI <$> kb i <*> pure p
  DisjE i j k -> disjE <$> kb i <*> kb j <*> kb k <*> pure p
  ImplE i j -> implE <$> kb i <*> kb j <*> pure p
  NegI i -> negI <$> kb i <*> pure p
  BotI i j -> botI <$> kb i <*> kb j <*> pure p
  BotE i -> botE <$> kb i <*> pure p
  NegNegI i -> negnegI <$> kb i <*> pure p
  NegNegE i -> negnegE <$> kb i <*> pure p

-- Check proof validity: whether every rule application is correct
checkProof :: Proof -> WithLogMayFail Checker
checkProof = foldM stepChecker initChecker

-- Check proof syntax: proof structure
--
-- Proof -> prems Steps
-- prems -> epsilon | AddPremise prems
-- Steps -> epsilon | IntrAsump Steps ElimAsump | ApplyRule Steps
checkProofSyntax :: Proof -> WithLogMayFail ()
checkProofSyntax steps = premFst >> asElim
  where
    premFst = do
      let isPrem step = case step of AddPremise _ -> True; _ -> False
      let body = dropWhile isPrem steps
      let ok = not $ any isPrem body
      let msg = "Syntax Error: Premises can only introduced at the beginning"
      checkCond msg ok
    asElim = do
      let ok = pairup == Just 0
      let msg = "Syntax Error: Assumption introductions-eliminations must be matched"
      checkCond msg ok
    pairup = foldM f 0 steps
      where
        f cnt (IntrAsump _) = Just $ cnt + 1
        f cnt (ElimAsump _) = if cnt > 0 then Just $ cnt - 1 else Nothing
        f cnt _ = Just cnt

r steps = premiseFirst && assumptionMatch
  where
    premiseFirst = not $ any isPrem $ dropWhile isPrem steps
    assumptionMatch = (0, 0) == foldl matchIter (0, 0) steps
    matchIter (m, e) (IntrAsump _) = (m, e + 1)
    matchIter (m, e) (ElimAsump _) = (min m (e - 1), e - 1)
    matchIter me _ = me
    isPrem (AddPremise _) = True
    isPrem _ = False

logLine :: Checker -> WithLogMayFail ()
logLine checker = label >> indent >> tell " "
  where
    indent = tell $ replicate level '\t'
    level = length (asumps checker)
    label = tell "L" >> tell (show line)
    line = ln checker

stepChecker :: Checker -> Step -> WithLogMayFail Checker
stepChecker checker@Checker {prems = prems, asumps = asumps, ctx = ctx, ln = ln, kb = kb} step = do
  logLine checker
  tell (show step)
  tell "\n"

  -- check validity
  case step of
    ApplyRule p rule ->
      let errMsg = "Cannot derive " ++ show p ++ " via " ++ show rule
       in checkRule (mkFinder kb) p rule >>= checkCond errMsg
    ElimAsump p ->
      let errMsg = "Never derived " ++ show p ++ "in current context"
          match (ctx', _, p') = ctx' <= ctx && p' == p
          matched = isJust $ find match kb
       in checkCond errMsg matched
    _ -> pure ()

  -- update knowledge base
  let checker' = case step of
        AddPremise p -> checker {prems = p : prems, kb = (0, ln, p) : kb}
        ApplyRule p rule -> checker {kb = (ctx, ln, p) : kb}
        IntrAsump p -> checker {asumps = p : asumps, kb = (ctx + 1, ln, p) : kb, ctx = ctx + 1}
        ElimAsump p ->
          let (a : asumps') = asumps
              p' = a `Impl` p
              curCtx (ctx', _, _) = ctx' == ctx
              kb' = dropWhile curCtx kb
           in checker {asumps = asumps', kb = (ctx - 1, ln, p') : kb', ctx = ctx - 1}
  pure checker' {ln = ln + 1}

maybeToExcept :: (Monad m) => Maybe a -> Err -> ExceptT Err m a
maybeToExcept (Just x) _ = ExceptT . pure $ Right x
maybeToExcept Nothing err = ExceptT . pure $ Left err

checkCond :: Err -> Bool -> WithLogMayFail ()
checkCond _ True = pure ()
checkCond err False = ExceptT . pure $ Left err
