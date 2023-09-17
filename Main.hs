module Main where

import Check
import Proof
import Prop
import Rule

p = Atom (AtomId 1)

q = Atom (AtomId 2)

r = Atom (AtomId 3)

-- (p -> q) |- (r or p) -> (r or q)
pf =
  Proof
    [ WithPremise [p `Impl` q],
      MakeAssumption
        (r `Or` p)
        (r `Or` q)
        [ MakeAssumption
            r
            (r `Or` p)
            [ ApplyRule r Assumption [],
              ApplyRule (r `Or` p) DisjI [StepRef 0]
            ],
          MakeAssumption
            p
            (r `Or` q)
            [ ApplyRule p Assumption [],
              ApplyRule (p `Impl` q) Premise [],
              ApplyRule q ImplE [StepRef 0, StepRef 1],
              ApplyRule (r `Or` q) DisjI [StepRef 2]
            ]
        ],
      ApplyRule ((r `Or` p) `Impl` (r `Or` q)) ImplI [StepRef 1]
    ]

main :: IO ()
main = print $ checkProof pf
