{-# LANGUAGE BlockArguments #-}

module Main where

import Check
import Control.Monad.Except
import Control.Monad.Writer
import Proof
import Prop
import Rules

-- Examples from chapter 1 of the LICS book
-- @book{huth2004logic
--   title={Logic in Computer Science: Modelling and reasoning about systems}
--   author={Huth, Michael and Ryan, Mark}
--   year={2004}
--   publisher={Cambridge university press}
-- }

nn p = Not $ Not p

[p, q, r, s, t] = Atom <$> [1 .. 5]

test :: String -> Builder -> (String, Proof)
test name builder = (name, build builder)

eg4 =
  test
    "Example 1.4"
    $ do
      addPremise $ p `And` q
      addPremise r
      applyRule q $ conjE 1
      applyRule (q `And` r) $ conjI 3 2

eg5 =
  test
    "Example 1.5"
    $ do
      addPremise p
      addPremise $ nn (q `And` r)
      applyRule (nn p) $ negnegI 1
      applyRule (q `And` r) $ negnegE 2
      applyRule r $ conjE 4
      applyRule (nn p `And` r) $ conjI 3 5

eg6 =
  test
    "Example 1.6"
    $ do
      addPremise $ (p `And` q) `And` r
      addPremise $ s `And` t
      applyRule (p `And` q) $ conjE 1
      applyRule q $ conjE 3
      applyRule s $ conjE 2
      applyRule (q `And` s) $ conjI 4 5

eg7 =
  test
    "Example 1.7"
    $ do
      addPremise $ p `Impl` (q `Impl` r)
      addPremise p
      addPremise $ Not r
      applyRule (q `Impl` r) $ implE 2 1
      intrAsump q
      applyRule r $ implE 5 4
      applyRule Bottom $ botI 6 3
      elimAsump Bottom
      applyRule (Not q) $ negI 8

eg8a =
  test
    "Example 1.8 (part a)"
    $ do
      addPremise $ Not p `Impl` q
      addPremise $ Not q
      intrAsump $ Not p
      applyRule q $ implE 3 1
      applyRule Bottom $ botI 4 2
      elimAsump Bottom
      applyRule (nn p) $ negI 6
      applyRule p $ negnegE 7

eg8b =
  test
    "Example 1.8 (part b)"
    $ do
      addPremise $ p `Impl` Not q
      addPremise q
      applyRule (nn q) $ negnegI 2
      intrAsump p
      applyRule (Not q) $ implE 4 1
      applyRule Bottom $ botI 2 5
      elimAsump Bottom
      applyRule (Not p) $ negI 7

eg9 =
  test
    "Example 1.9"
    $ do
      addPremise $ Not q `Impl` Not p
      intrAsump p
      intrAsump $ Not q
      applyRule (Not p) $ implE 3 1
      applyRule Bottom $ botI 2 4
      elimAsump Bottom
      applyRule (nn q) $ negI 6
      elimAsump $ nn q

egOneline = test "Identity Law" $ do intrAsump p; elimAsump p

eg11 =
  test
    "Example 1.11"
    $ do
      intrAsump $ q `Impl` r
      intrAsump $ Not q `Impl` Not p
      intrAsump p
      intrAsump $ Not q
      applyRule (Not p) $ implE 4 2
      applyRule Bottom $ botI 3 5
      elimAsump Bottom
      applyRule (nn q) $ negI 7
      applyRule q $ negnegE 8
      applyRule r $ implE 9 1
      elimAsump r
      elimAsump $ p `Impl` r
      elimAsump $ (Not q `Impl` Not p) `Impl` (p `Impl` r)

eg13 =
  test
    "Example 1.13"
    $ do
      addPremise $ (p `And` q) `Impl` r
      intrAsump p
      intrAsump q
      applyRule (p `And` q) $ conjI 2 3
      applyRule r $ implE 4 1
      elimAsump r
      elimAsump $ q `Impl` r

eg14 =
  test
    "Example 1.14"
    $ do
      addPremise $ p `Impl` (q `Impl` r)
      intrAsump $ p `And` q
      applyRule p $ conjE 2
      applyRule q $ conjE 2
      applyRule (q `Impl` r) $ implE 3 1
      applyRule r $ implE 4 5
      elimAsump r

eg15 =
  test
    "Example 1.15"
    $ do
      addPremise $ p `Impl` q
      intrAsump $ p `And` r
      applyRule p $ conjE 2
      applyRule r $ conjE 2
      applyRule q $ implE 3 1
      applyRule (q `And` r) $ conjI 5 4
      elimAsump $ q `And` r

eg16 =
  test
    "Example 1.16"
    $ do
      addPremise $ q `Impl` r
      intrAsump $ p `Or` q
      intrAsump p
      applyRule (p `Or` r) $ disjI 3
      elimAsump $ p `Or` r
      intrAsump q
      applyRule r $ implE 6 1
      applyRule (p `Or` r) $ disjI 7
      elimAsump $ p `Or` r
      applyRule (p `Or` r) $ disjE 2 5 9
      elimAsump $ p `Or` r

eg17 =
  test
    "Example 1.17"
    $ do
      addPremise $ (p `Or` q) `Or` r
      intrAsump $ p `Or` q
      intrAsump p
      applyRule (p `Or` (q `Or` r)) $ disjI 3
      elimAsump $ p `Or` (q `Or` r)
      intrAsump q
      applyRule (q `Or` r) $ disjI 6
      applyRule (p `Or` (q `Or` r)) $ disjI 7
      elimAsump $ p `Or` (q `Or` r)
      applyRule (p `Or` (q `Or` r)) $ disjE 2 5 9
      elimAsump $ p `Or` (q `Or` r)
      intrAsump r
      applyRule (q `Or` r) $ disjI 12
      applyRule (p `Or` (q `Or` r)) $ disjI 13
      elimAsump $ p `Or` (q `Or` r)
      applyRule (p `Or` (q `Or` r)) $ disjE 1 11 15

eg18 =
  test
    "Example 1.18"
    $ do
      addPremise $ p `And` (q `Or` r)
      applyRule p $ conjE 1
      applyRule (q `Or` r) $ conjE 1
      intrAsump q
      applyRule (p `And` q) $ conjI 2 4
      applyRule ((p `And` q) `Or` (p `And` r)) $ disjI 5
      elimAsump $ (p `And` q) `Or` (p `And` r)
      intrAsump r
      applyRule (p `And` r) $ conjI 2 8
      applyRule ((p `And` q) `Or` (p `And` r)) $ disjI 9
      elimAsump $ (p `And` q) `Or` (p `And` r)
      applyRule ((p `And` q) `Or` (p `And` r)) $ disjE 3 7 11

egcopy =
  test
    "Example copy"
    $ do
      intrAsump p
      intrAsump q
      elimAsump p
      elimAsump $ q `Impl` p

eg20 =
  test
    "Example 1.20"
    $ do
      addPremise $ Not p `Or` q
      intrAsump $ Not p
      intrAsump p
      applyRule Bottom $ botI 3 2
      applyRule q $ botE 4
      elimAsump q
      elimAsump $ p `Impl` q
      intrAsump q
      intrAsump p
      elimAsump q
      elimAsump $ p `Impl` q
      applyRule (p `Impl` q) $ disjE 1 7 11

eg21 =
  test
    "Example 1.21"
    $ do
      addPremise $ p `Impl` q
      addPremise $ p `Impl` Not q
      intrAsump p
      applyRule q $ implE 3 1
      applyRule (Not q) $ implE 3 2
      applyRule Bottom $ botI 4 5
      elimAsump Bottom
      applyRule (Not p) $ negI 7

egcontradict =
  test
    "Example Contradicting Implication"
    $ do
      addPremise $ p `Impl` Not p
      intrAsump p
      applyRule (Not p) $ implE 2 1
      applyRule Bottom $ botI 2 3
      elimAsump Bottom
      applyRule (Not p) $ negI 5

eg22 =
  test
    "Example 1.22"
    $ do
      addPremise $ p `Impl` (q `Impl` r)
      addPremise p
      addPremise $ Not r
      applyRule (q `Impl` r) $ implE 2 1
      intrAsump q
      applyRule r $ implE 5 4
      applyRule Bottom $ botI 6 3
      elimAsump Bottom
      applyRule (Not q) $ negI 8

eg23 =
  test
    "Example 1.23"
    $ do
      addPremise $ (p `And` Not q) `Impl` r
      addPremise $ Not r
      addPremise p
      intrAsump $ Not q
      applyRule (p `And` Not q) $ conjI 3 4
      applyRule r $ implE 5 1
      applyRule Bottom $ botI 6 2
      elimAsump Bottom
      applyRule (nn q) $ negI 8
      applyRule q $ negnegE 9

egLEM =
  test
    "Example Excluded Middle"
    $ do
      intrAsump $ Not (p `Or` Not p)
      intrAsump p
      applyRule (p `Or` Not p) $ disjI 2
      applyRule Bottom $ botI 3 1
      elimAsump Bottom
      applyRule (Not p) $ negI 5
      applyRule (p `Or` Not p) $ disjI 6
      applyRule Bottom $ botI 7 1
      elimAsump Bottom
      applyRule (nn (p `Or` Not p)) $ negI 9
      applyRule (p `Or` Not p) $ negnegE 10

eg24 =
  test
    "Example 1.24"
    $ do
      addPremise $ p `Impl` q
      intrAsump $ Not (p `Or` Not p)
      intrAsump p
      applyRule (p `Or` Not p) $ disjI 3
      applyRule Bottom $ botI 4 2
      elimAsump Bottom
      applyRule (Not p) $ negI 6
      applyRule (p `Or` Not p) $ disjI 7
      applyRule Bottom $ botI 8 2
      elimAsump Bottom
      applyRule (nn (p `Or` Not p)) $ negI 10
      applyRule (p `Or` Not p) $ negnegE 11
      intrAsump p
      applyRule q $ implE 13 1
      applyRule (Not p `Or` q) $ disjI 14
      elimAsump (Not p `Or` q)
      intrAsump (Not p)
      applyRule (Not p `Or` q) $ disjI 17
      elimAsump (Not p `Or` q)
      applyRule (Not p `Or` q) $ disjE 12 16 19

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
    [ test "syntax-wrong-1" $ do intrAsump p,
      test "syntax-wrong-2" $ do elimAsump p,
      test "syntax-wrong-3" $ do applyRule p $ botE 1; addPremise p
    ]

  putStrLn "## BEGIN Examples ##"
  runTests examples
  putStrLn "## END Examples ##"

  putStrLn "## BEGIN Exercises ##"
  runTests exercises
  putStrLn "## END Exercises ##"
