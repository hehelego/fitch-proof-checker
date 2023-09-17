module Proof where

import Prop (Prop)

data Rule
  = Premise -- use premise
  | Assumption -- use assumption
  | ConjI -- conjunction introduction
  | ConjE -- conjunction elimination
  | DisjI -- disjunction introduction
  | DisjE -- disjunction elimination
  | ImplI -- implication introduction
  | ImplE -- implication elimination
  | NegI -- negation introduction
  | NegE -- negation elimination
  | BotE -- bottom elimination
  | NegNegI -- double negation introduction
  | NegNegE -- double negation elimination
  deriving (Show, Eq, Ord)

newtype StepRef = StepRef Int

data Step
  = MakeAssumption Prop Prop [Step]
  | ApplyRule Prop Rule [StepRef]

data Proof = Proof {pfPremises :: [Prop], pfSteps :: [Step]}

-- TODO: implement proof syntax check
-- 1. A `Proof` must begin with a `WithPremise` step and end with a single conclusion proposition.
-- 2. A `WithAssumption` block must begin with an assumption proposition and end with an colucsion proposition, which are equal to the two parameters passed to the `MakeAssumption` value constructor.
checkProofSyntax :: Proof -> Bool
checkProofSyntax _ = True
