module Proof where

import Prop (Prop)

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
  | ImplI StepRef -- implication introduction
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
  show (ImplI p_q) = "[→i: " ++ show p_q ++ "]"
  show (ImplE p p_q) = "[→e: " ++ show p ++ ", " ++ show p_q ++ "]"
  show (NegI p_bot) = "[¬i: " ++ show p_bot ++ "]"
  show (BotI p not_p) = "[⊥ i: " ++ show p ++ ", " ++ show not_p ++ "]"
  show (BotE bot) = "[⊥ e: " ++ show bot ++ "]"
  show (NegNegI p) = "[¬¬e: " ++ show p ++ "]"
  show (NegNegE nnp) = "[¬¬i: " ++ show nnp ++ "]"

data StepRef = SingleRef Int | BlockRef Int Int deriving (Eq, Ord)

instance Show StepRef where
  show (SingleRef i) = show i
  show (BlockRef i j) = "[" ++ show i ++ ", " ++ show j ++ "]"

data Step
  = AddPremise Prop
  | Assume Prop
  | EndAssumption
  | ApplyRule Prop Rule
  deriving (Eq)

instance Show Step where
  show (AddPremise p) = "Premise " ++ show p
  show (Assume p) = "Assume " ++ show p
  show EndAssumption = "EndAssumption"
  show (ApplyRule p r) = "Deduce " ++ show p ++ " " ++ show r

newtype Proof = Proof [Step]
