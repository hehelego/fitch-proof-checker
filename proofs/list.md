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

## 3.fitch 4.fitch

- $\vdash p \lor q \lor r$
- expected result:
    - parser correctly
    - valid structure
    - incorrect proof

**bug**: failed enforce block visibility

**fixed**: Commit `fc7529d7fe762943a0600603d631722e3b8a7f89`

## 5.fitch

Incorrect rule application

- $p\vdash p \land q$
- expected result:
    - parser correctly
    - valid structure
    - incorrect proof

## 6.fitch

Refering to a unknown step

- $\vdash p \lor q$
- expected result:
    - parser correctly
    - valid structure
    - incorrect proof
