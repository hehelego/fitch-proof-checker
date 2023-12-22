# Revision history for fitch-proof-checker

## 0.1.0.0 -- 2023-10-13

* Make `fitch-proof-checker` a cabal project

## 0.2.0.0 -- 2023-10-27

* Implement a simple and naive parser for Fitch notation
* Add two example proofs

## 0.3.0.0 -- 2023-10-29

* Add error message and log on check failure
* Bug fix: scope visibility is now correctly enforced
* Add 4 test cases

## 0.4.0.0 -- 2023-12-22

* Rebuild the proof DSL
* Add a proof builder monad for writing proof
* `CheckProofSyntax` with monad: `ExceptT String (Writer String) ()`
* Implement all natural deduction proof examples in LICS chapter 1
