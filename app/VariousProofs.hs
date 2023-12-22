{-# LANGUAGE BlockArguments #-}

module Main where

import Check
import Control.Monad.Except
import Control.Monad.Writer
import Proof
import Prop

-- Examples from chapter 1 of the LICS book
-- @book{huth2004logic,
--   title={Logic in Computer Science: Modelling and reasoning about systems},
--   author={Huth, Michael and Ryan, Mark},
--   year={2004},
--   publisher={Cambridge university press}
-- }

nn p = Not $ Not p

[p, q, r, s, t] = Atom <$> [1 .. 5]

test :: String -> Proof -> (String, Proof)
test = (,)

eg4 =
  test
    "Example 1.4"
    [ AddPremise $ p `And` q,
      AddPremise r,
      ApplyRule q $ ConjE 1,
      ApplyRule (q `And` r) $ ConjI 3 2
    ]

eg5 =
  test
    "Example 1.5"
    [ AddPremise p,
      AddPremise $ nn (q `And` r),
      ApplyRule (nn p) $ NegNegI 1,
      ApplyRule (q `And` r) $ NegNegE 2,
      ApplyRule r $ ConjE 4,
      ApplyRule (nn p `And` r) $ ConjI 3 5
    ]

eg6 =
  test
    "Example 1.6"
    [ AddPremise $ (p `And` q) `And` r,
      AddPremise $ s `And` t,
      ApplyRule (p `And` q) $ ConjE 1,
      ApplyRule q $ ConjE 3,
      ApplyRule s $ ConjE 2,
      ApplyRule (q `And` s) $ ConjI 4 5
    ]

eg7 =
  test
    "Example 1.7"
    [ AddPremise $ p `Impl` (q `Impl` r),
      AddPremise p,
      AddPremise $ Not r,
      ApplyRule (q `Impl` r) $ ImplE 2 1,
      IntrAsump q,
      ApplyRule r $ ImplE 5 4,
      ApplyRule Bottom $ BotI 6 3,
      ElimAsump Bottom,
      ApplyRule (Not q) $ NegI 8
    ]

eg8a =
  test
    "Example 1.8 (part a)"
    [ AddPremise $ Not p `Impl` q,
      AddPremise $ Not q,
      IntrAsump $ Not p,
      ApplyRule q $ ImplE 3 1,
      ApplyRule Bottom $ BotI 4 2,
      ElimAsump Bottom,
      ApplyRule (nn p) $ NegI 6,
      ApplyRule p $ NegNegE 7
    ]

eg8b =
  test
    "Example 1.8 (part b)"
    [ AddPremise $ p `Impl` Not q,
      AddPremise q,
      ApplyRule (nn q) $ NegNegI 2,
      IntrAsump p,
      ApplyRule (Not q) $ ImplE 4 1,
      ApplyRule Bottom $ BotI 2 5,
      ElimAsump Bottom,
      ApplyRule (Not p) $ NegI 7
    ]

eg9 =
  test
    "Example 1.9"
    [ AddPremise $ Not q `Impl` Not p,
      IntrAsump p,
      IntrAsump $ Not q,
      ApplyRule (Not p) $ ImplE 3 1,
      ApplyRule Bottom $ BotI 2 4,
      ElimAsump Bottom,
      ApplyRule (nn q) $ NegI 6,
      ElimAsump $ nn q
    ]

egOneline = test "Identity Law" [IntrAsump p, ElimAsump p]

eg11 =
  test
    "Example 1.11"
    [ IntrAsump $ q `Impl` r,
      IntrAsump $ Not q `Impl` Not p,
      IntrAsump p,
      IntrAsump $ Not q,
      ApplyRule (Not p) $ ImplE 4 2,
      ApplyRule Bottom $ BotI 3 5,
      ElimAsump Bottom,
      ApplyRule (nn q) $ NegI 7,
      ApplyRule q $ NegNegE 8,
      ApplyRule r $ ImplE 9 1,
      ElimAsump r,
      ElimAsump $ p `Impl` r,
      ElimAsump $ (Not q `Impl` Not p) `Impl` (p `Impl` r)
    ]

eg13 =
  test
    "Example 1.13"
    [ AddPremise $ (p `And` q) `Impl` r,
      IntrAsump p,
      IntrAsump q,
      ApplyRule (p `And` q) $ ConjI 2 3,
      ApplyRule r $ ImplE 4 1,
      ElimAsump r,
      ElimAsump $ q `Impl` r
    ]

eg14 =
  test
    "Example 1.14"
    [ AddPremise $ p `Impl` (q `Impl` r),
      IntrAsump $ p `And` q,
      ApplyRule p $ ConjE 2,
      ApplyRule q $ ConjE 2,
      ApplyRule (q `Impl` r) $ ImplE 3 1,
      ApplyRule r $ ImplE 4 5,
      ElimAsump r
    ]

eg15 =
  test
    "Example 1.15"
    [ AddPremise $ p `Impl` q,
      IntrAsump $ p `And` r,
      ApplyRule p $ ConjE 2,
      ApplyRule r $ ConjE 2,
      ApplyRule q $ ImplE 3 1,
      ApplyRule (q `And` r) $ ConjI 5 4,
      ElimAsump $ q `And` r
    ]

eg16 =
  test
    "Example 1.16"
    [ AddPremise $ q `Impl` r,
      IntrAsump $ p `Or` q,
      IntrAsump p,
      ApplyRule (p `Or` r) $ DisjI 3,
      ElimAsump $ p `Or` r,
      IntrAsump q,
      ApplyRule r $ ImplE 6 1,
      ApplyRule (p `Or` r) $ DisjI 7,
      ElimAsump $ p `Or` r,
      ApplyRule (p `Or` r) $ DisjE 2 5 9,
      ElimAsump $ p `Or` r
    ]

eg17 =
  test
    "Example 1.17"
    [ AddPremise $ (p `Or` q) `Or` r,
      IntrAsump $ p `Or` q,
      IntrAsump p,
      ApplyRule (p `Or` (q `Or` r)) $ DisjI 3,
      ElimAsump $ p `Or` (q `Or` r),
      IntrAsump q,
      ApplyRule (q `Or` r) $ DisjI 6,
      ApplyRule (p `Or` (q `Or` r)) $ DisjI 7,
      ElimAsump $ p `Or` (q `Or` r),
      ApplyRule (p `Or` (q `Or` r)) $ DisjE 2 5 9,
      ElimAsump $ p `Or` (q `Or` r),
      IntrAsump r,
      ApplyRule (q `Or` r) $ DisjI 12,
      ApplyRule (p `Or` (q `Or` r)) $ DisjI 13,
      ElimAsump $ p `Or` (q `Or` r),
      ApplyRule (p `Or` (q `Or` r)) $ DisjE 1 11 15
    ]

eg18 =
  test
    "Example 1.18"
    [ AddPremise $ p `And` (q `Or` r),
      ApplyRule p $ ConjE 1,
      ApplyRule (q `Or` r) $ ConjE 1,
      IntrAsump q,
      ApplyRule (p `And` q) $ ConjI 2 4,
      ApplyRule ((p `And` q) `Or` (p `And` r)) $ DisjI 5,
      ElimAsump $ (p `And` q) `Or` (p `And` r),
      IntrAsump r,
      ApplyRule (p `And` r) $ ConjI 2 8,
      ApplyRule ((p `And` q) `Or` (p `And` r)) $ DisjI 9,
      ElimAsump $ (p `And` q) `Or` (p `And` r),
      ApplyRule ((p `And` q) `Or` (p `And` r)) $ DisjE 3 7 11
    ]

egcopy =
  test
    "Example copy"
    [ IntrAsump p,
      IntrAsump q,
      ElimAsump p,
      ElimAsump $ q `Impl` p
    ]

eg20 =
  test
    "Example 1.20"
    [ AddPremise $ Not p `Or` q,
      IntrAsump $ Not p,
      IntrAsump p,
      ApplyRule Bottom $ BotI 3 2,
      ApplyRule q $ BotE 4,
      ElimAsump q,
      ElimAsump $ p `Impl` q,
      IntrAsump q,
      IntrAsump p,
      ElimAsump q,
      ElimAsump $ p `Impl` q,
      ApplyRule (p `Impl` q) $ DisjE 1 7 11
    ]

eg21 =
  test
    "Example 1.21"
    [ AddPremise $ p `Impl` q,
      AddPremise $ p `Impl` Not q,
      IntrAsump p,
      ApplyRule q $ ImplE 3 1,
      ApplyRule (Not q) $ ImplE 3 2,
      ApplyRule Bottom $ BotI 4 5,
      ElimAsump Bottom,
      ApplyRule (Not p) $ NegI 7
    ]

egcontradict =
  test
    "Example Contradicting Implication"
    [ AddPremise $ p `Impl` Not p,
      IntrAsump p,
      ApplyRule (Not p) $ ImplE 2 1,
      ApplyRule Bottom $ BotI 2 3,
      ElimAsump Bottom,
      ApplyRule (Not p) $ NegI 5
    ]

eg22 =
  test
    "Example 1.22"
    [ AddPremise $ p `Impl` (q `Impl` r),
      AddPremise p,
      AddPremise $ Not r,
      ApplyRule (q `Impl` r) $ ImplE 2 1,
      IntrAsump q,
      ApplyRule r $ ImplE 5 4,
      ApplyRule Bottom $ BotI 6 3,
      ElimAsump Bottom,
      ApplyRule (Not q) $ NegI 8
    ]

eg23 =
  test
    "Example 1.23"
    [ AddPremise $ (p `And` Not q) `Impl` r,
      AddPremise $ Not r,
      AddPremise p,
      IntrAsump $ Not q,
      ApplyRule (p `And` Not q) $ ConjI 3 4,
      ApplyRule r $ ImplE 5 1,
      ApplyRule Bottom $ BotI 6 2,
      ElimAsump Bottom,
      ApplyRule (nn q) $ NegI 8,
      ApplyRule q $ NegNegE 9
    ]

egLEM =
  test
    "Example Excluded Middle"
    [ IntrAsump $ Not (p `Or` Not p),
      IntrAsump p,
      ApplyRule (p `Or` Not p) $ DisjI 2,
      ApplyRule Bottom $ BotI 3 1,
      ElimAsump Bottom,
      ApplyRule (Not p) $ NegI 5,
      ApplyRule (p `Or` Not p) $ DisjI 6,
      ApplyRule Bottom $ BotI 7 1,
      ElimAsump Bottom,
      ApplyRule (nn (p `Or` Not p)) $ NegI 9,
      ApplyRule (p `Or` Not p) $ NegNegE 10
    ]

eg24 =
  test
    "Example 1.24"
    [ AddPremise $ p `Impl` q,
      IntrAsump $ Not (p `Or` Not p),
      IntrAsump p,
      ApplyRule (p `Or` Not p) $ DisjI 3,
      ApplyRule Bottom $ BotI 4 2,
      ElimAsump Bottom,
      ApplyRule (Not p) $ NegI 6,
      ApplyRule (p `Or` Not p) $ DisjI 7,
      ApplyRule Bottom $ BotI 8 2,
      ElimAsump Bottom,
      ApplyRule (nn (p `Or` Not p)) $ NegI 10,
      ApplyRule (p `Or` Not p) $ NegNegE 11,
      IntrAsump p,
      ApplyRule q $ ImplE 13 1,
      ApplyRule (Not p `Or` q) $ DisjI 14,
      ElimAsump (Not p `Or` q),
      IntrAsump (Not p),
      ApplyRule (Not p `Or` q) $ DisjI 17,
      ElimAsump (Not p `Or` q),
      ApplyRule (Not p `Or` q) $ DisjE 12 16 19
    ]

-- examples
examples = [eg4, eg5, eg6, eg7, eg8a, eg8b, eg9, egOneline, eg11, eg13, eg14, eg15, eg16, eg17, eg18, egcopy, eg20, eg21, egcontradict, eg22, eg23, egLEM, eg24]

-- TODO: exercises

-- exercises
exercises = []

runTests :: [(String, Proof)] -> IO ()
runTests = mapM_ (\(name, pf) -> putStrLn name >> showPF pf >> checkPF pf >> putStrLn "\n")

showPF :: Proof -> IO ()
showPF = mapM_ print

checkPF :: Proof -> IO ()
checkPF proof =
  putStrLn log >> case ck of
    Right _ -> putStrLn "Correct"
    Left err -> putStr "Wrong" >> putStrLn err
  where
    run = runWriter . runExceptT
    (ck, log) = run $ checkProofSyntax proof >> checkProof proof

main :: IO ()
main = do
  runTests
    [ test "syntax-wrong-1" [IntrAsump p],
      test "syntax-wrong-2" [ElimAsump p],
      test "syntax-wrong-3" [ApplyRule p (BotE 1), AddPremise p]
    ]

  putStrLn "## BEGIN Examples ##"
  runTests examples
  putStrLn "## END Examples ##"

  putStrLn "## BEGIN Exercises ##"
  runTests exercises
  putStrLn "## END Exercises ##"
