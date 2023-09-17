module Proof where

import Prop (Prop)
import Rule (Rule)

newtype StepRef = StepRef Int

data Step
  = MakeAssumption Prop.Prop Prop.Prop [Step]
  | ApplyRule Prop.Prop Rule.Rule [StepRef]

data Proof = Proof {pfPremises :: [Prop], pfSteps :: [Step]}

-- TODO: implement proof syntax check
-- 1. A `Proof` must begin with a `WithPremise` step and end with a single conclusion proposition.
-- 2. A `WithAssumption` block must begin with an assumption proposition and end with an colucsion proposition, which are equal to the two parameters passed to the `MakeAssumption` value constructor.
checkProofSyntax :: Proof -> Bool
checkProofSyntax _ = True
