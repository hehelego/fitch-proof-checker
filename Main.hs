module Main where

import Check
import Proof
import Prop

[p, q, r, s, t, u, v, w] = Atom <$> [1 .. 8]

-- (p -> q) |- (r or p) -> (r or q)
pf =
  Proof
    [ AddPremise (p `Impl` q), -- 1
      Assume (r `Or` p), -- 2:7
      Assume r, -- 3:4
      ApplyRule (r `Or` q) (DisjI (SingleRef 3)), -- 4
      EndAssumption,
      Assume p, -- 5:7
      ApplyRule q (ImplE (SingleRef 5) (SingleRef 1)), -- 6
      ApplyRule (r `Or` q) (DisjI (SingleRef 6)), -- 7
      EndAssumption,
      ApplyRule (r `Or` q) (DisjE (SingleRef 2) (BlockRef 3 4) (BlockRef 5 7)), -- 8
      EndAssumption,
      ApplyRule ((r `Or` p) `Impl` (r `Or` q)) (ImplI (BlockRef 2 8)) -- 9
    ]

main :: IO ()
main = print $ checkProof pf
