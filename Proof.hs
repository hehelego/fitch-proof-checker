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
  = Premise -- use premise
  | Assumption -- use assumption
  | ConjI StepRef StepRef -- conjunction introduction
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

type StepRef = Int

data Step
  = MakeAssumption Prop Prop [Step]
  | ApplyRule Prop Rule

data Proof = Proof {pfPremises :: [Prop], pfSteps :: [Step]}

-- TODO: implement proof syntax check
-- 1. A `Proof` must begin with a `WithPremise` step and end with a single conclusion proposition.
-- 2. A `WithAssumption` block must begin with an assumption proposition and end with an colucsion proposition, which are equal to the two parameters passed to the `MakeAssumption` value constructor.
checkProofSyntax :: Proof -> Bool
checkProofSyntax _ = True
