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

```
Assume p
    Derive p OR q  Intr_Disj 1
EndAssumption
Derive p OR q OR r Intr_Disj 2
```

