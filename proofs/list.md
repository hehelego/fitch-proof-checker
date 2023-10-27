# list of example proofs

To check a proof, run `cabal run fitch-proof-checker -- proofs/file.fitch`

## 1.fitch

- $(p \to q) \vdash (r \lor p) -> (r \lor q)$
- expected result:
    - parse correctly
    - valid structure
    - correct proof

## 2.fitch

- $(p\to q), \lnot (p\to q) \vdash \bot \land (p\or \lnot p)$
- expected result:
    - parse correctly
    - valid structure
    - correct proof
