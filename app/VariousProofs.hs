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

-- TODO: complete examples
eg15 = test "Example 1.15" undefined

eg16 = test "Example 1.16" undefined

eg17 = test "Example 1.17" undefined

eg18 = test "Example 1.18" undefined

eg20 = test "Example 1.20" undefined

eg21 = test "Example 1.21" undefined

eg22 = test "Example 1.22" undefined

eg23 = test "Example 1.23" undefined

eg24 = test "Example 1.24" undefined

eg29 = test "Example 1.29" undefined

-- examples
examples = [eg4, eg5, eg6, eg7, eg8a, eg8b, eg9, egOneline, eg11, eg13, eg14, eg15, eg16, eg17, eg18, eg20, eg21, eg22, eg23, eg24, eg29]

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
