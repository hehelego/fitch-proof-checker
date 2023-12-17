module Proof
  ( Rule (ConjI, ConjE, DisjI, DisjE, ImplE, NegI, BotI, BotE, NegNegI, NegNegE),
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

-- These notations come from section 1.2.3 of the book `Logic in Computer Science`
-- @book{huth2004logic,
--   title={Logic in Computer Science: Modelling and reasoning about systems},
--   author={Huth, Michael and Ryan, Mark},
--   year={2004},
--   publisher={Cambridge university press}
-- }
data Rule
  = ConjI StepRef StepRef -- conjunction introduction
  | ConjE StepRef -- conjunction elimination
  | DisjI StepRef -- disjunction introduction
  | DisjE StepRef StepRef StepRef -- disjunction elimination
  | ImplE StepRef StepRef -- implication elimination
  | NegI StepRef -- negation introduction
  | BotI StepRef StepRef -- bottom introduction
  | BotE StepRef -- bottom elimination
  | NegNegI StepRef -- double negation introduction
  | NegNegE StepRef -- double negation elimination
  deriving (Eq, Ord)

instance Show Rule where
  show (ConjI p q) = "[∧i: " ++ show p ++ ", " ++ show q ++ "]"
  show (ConjE pq) = "[∧e: " ++ show pq ++ "]"
  show (DisjI p_q) = "[∨i: " ++ show p_q ++ "]"
  show (DisjE pq p_x q_x) = "[∨e: " ++ show pq ++ ", " ++ show p_x ++ ", " ++ show q_x ++ "]"
  show (ImplE p p_q) = "[→e: " ++ show p ++ ", " ++ show p_q ++ "]"
  show (NegI p_bot) = "[¬i: " ++ show p_bot ++ "]"
  show (BotI p not_p) = "[⊥ i: " ++ show p ++ ", " ++ show not_p ++ "]"
  show (BotE bot) = "[⊥ e: " ++ show bot ++ "]"
  show (NegNegI p) = "[¬¬e: " ++ show p ++ "]"
  show (NegNegE nnp) = "[¬¬i: " ++ show nnp ++ "]"

type StepRef = Int

data Step
  = AddPremise Prop
  | IntrAsump Prop
  | ElimAsump Prop
  | ApplyRule Prop Rule
  deriving (Eq)

instance Show Step where
  show (AddPremise p) = "Premise " ++ show p
  show (IntrAsump p) = "Assume " ++ show p
  show (ElimAsump p) = "Derive " ++ show p ++ "with assumptions"
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

applyRule :: Prop -> Rule -> Builder
applyRule p rule = continue $ ApplyRule p rule

intrAsump :: Prop -> Builder
intrAsump p = continue $ IntrAsump p

elimAsump :: Prop -> Builder
elimAsump p = continue $ ElimAsump p

endProof :: Builder
endProof = pure 0

build :: Builder -> Proof
build builder = snd $ evalState (runWriterT builder) 0
