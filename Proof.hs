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
    | NegE StepRef StepRef -- negation elimination
    | BotE StepRef -- bottom elimination
    | NegNegI StepRef -- double negation introduction
    | NegNegE StepRef -- double negation elimination
    deriving (Show, Eq, Ord)

data StepRef = SingleRef Int | BlockRef Int Int deriving (Show, Eq, Ord)

data Step
    = AddPremise Prop
    | Assume Prop
    | EndAssumption
    | ApplyRule Prop Rule

newtype Proof = Proof [Step]

-- TODO: implement proof syntax check
-- TODO: implement proof parser
-- Proof -> Premises Steps
-- Premises -> \epsilon | proposition Premises
-- Steps -> \epsilon | makeAssumption Steps endAssumption | applyRule Step
checkProofSyntax :: Proof -> Bool
checkProofSyntax _ = True
