module Proof
  ( Arity0Rule (Arity0Rule),
    Arity1Rule (Arity1Rule),
    Arity2Rule (Arity2Rule),
    Arity3Rule (Arity3Rule),
    RuleApp (A0, A1, A2, A3),
    StepRef,
    Step (AddPremise, IntrAsump, ElimAsump, ApplyRule),
    Proof,
    Builder,
    addPremise,
    applyRule,
    intrAsump,
    elimAsump,
    endProof,
    build,
  )
where

import Control.Monad.State.Lazy
import Control.Monad.Writer.Lazy
import Data.Functor
import Prop (Prop (Atom))

data Arity0Rule = Arity0Rule String (Prop -> Bool)

data Arity1Rule = Arity1Rule String (Prop -> Prop -> Bool)

data Arity2Rule = Arity2Rule String (Prop -> Prop -> Prop -> Bool)

data Arity3Rule = Arity3Rule String (Prop -> Prop -> Prop -> Prop -> Bool)

-- Rule Application
data RuleApp
  = A0 Arity0Rule
  | A1 Arity1Rule StepRef
  | A2 Arity2Rule StepRef StepRef
  | A3 Arity3Rule StepRef StepRef StepRef

--   = ConjI StepRef StepRef -- conjunction introduction
--   | ConjE StepRef -- conjunction elimination
--   | DisjI StepRef -- disjunction introduction
--   | DisjE StepRef StepRef StepRef -- disjunction elimination
--   | ImplE StepRef StepRef -- implication elimination
--   | NegI StepRef -- negation introduction
--   | BotI StepRef StepRef -- bottom introduction
--   | BotE StepRef -- bottom elimination
--   | NegNegI StepRef -- double negation introduction
--   | NegNegE StepRef -- double negation elimination
--   deriving (Eq, Ord)
--
instance Show RuleApp where
  show (A0 (Arity0Rule desc _)) = "[" ++ desc ++ "]"
  show (A1 (Arity1Rule desc _) i) = "[" ++ desc ++ " from " ++ show i ++ "]"
  show (A2 (Arity2Rule desc _) i j) = "[" ++ desc ++ " from " ++ show i ++ ", " ++ show j ++ "]"
  show (A3 (Arity3Rule desc _) i j k) = "[" ++ desc ++ " from " ++ show i ++ ", " ++ show j ++ ", " ++ show k ++ "]"

type StepRef = Int

data Step
  = AddPremise Prop
  | IntrAsump Prop
  | ElimAsump Prop
  | ApplyRule Prop RuleApp

instance Show Step where
  show (AddPremise p) = "Premise " ++ show p
  show (IntrAsump p) = "Assume " ++ show p
  show (ElimAsump p) = "Derive " ++ show p ++ " with assumptions"
  show (ApplyRule p r) = "Deduce " ++ show p ++ " " ++ show r

type Proof = [Step]

-- Builder: accumulate steps & count line number
type Builder = WriterT [Step] (State Int) Int

continue :: Step -> Builder
continue s = do
  tell [s]
  ln <- get
  return $ 1 + ln

addPremise :: Prop -> Builder
addPremise p = continue $ AddPremise p

applyRule :: Prop -> RuleApp -> Builder
applyRule p rule = continue $ ApplyRule p rule

intrAsump :: Prop -> Builder
intrAsump p = continue $ IntrAsump p

elimAsump :: Prop -> Builder
elimAsump p = continue $ ElimAsump p

endProof :: Builder
endProof = pure 0

build :: Builder -> Proof
build builder = snd $ evalState (runWriterT builder) 0
