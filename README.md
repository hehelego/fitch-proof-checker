# fitch-proof-checker

(Work in progress) A proof checker for natural deduction proofs written in Fitch notation

## notations

- English letters: propositon variables
- Greek letters: well-formed formula
- $\land$: conjunction, logical and.
- $\lor$: disjunction, logical or.
- $\lnot$: negation, logical not.
- $\to$: implication.
- $\vdash$: syntactic consequence, proves.
- $\vDash$: semantic consequence, entails.

## rules of inferences

Basic rules:

|                              | Introduction rule                                                                  | Elimination rule                                                                                                                                       |
|------------------------------|------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------|
| Conjunction $\land$          | $\dfrac{\phi \quad \psi}{\phi \land \psi}\ \land_i$                                | $\dfrac{\phi \land \psi}{\phi}\ \land_e$ and $\dfrac{\phi \land \psi}{\psi}\ \land_e$                                                                  |
| Disjunction $\lor$           | $\dfrac{\phi}{\phi \lor \psi}\ \lor_i$ and $\dfrac{\psi}{\phi \lor \psi}\ \lor_i$  | $\dfrac{\begin{matrix}\\ \\ \phi \lor \psi\end{matrix}\quad \begin{bmatrix}\phi \\ \vdots \\ \chi\end{bmatrix}\quad \begin{bmatrix}\psi \\ \vdots \\ \chi\end{bmatrix}}{\chi}\ \lor_e$ |
| Implication $\to$            | $\dfrac{\begin{bmatrix}\phi \\ \vdots \\ \psi\end{bmatrix}}{\phi \to \psi}\ \to_i$ | $\dfrac{\phi \quad \phi\to\psi}{\psi}\ \to_e$                                                                                                          |
| Negation $\lnot$             | $\dfrac{\begin{bmatrix}\phi \\ \vdots \\ \bot\end{bmatrix}}{\lnot\phi}\ \lnot_i$   | Not Applicable                                                                                                                                         |
| Double Negation $\lnot\lnot$ | $\dfrac{\phi}{\lnot\lnot\phi}\ \lnot\lnot_i$                                       | $\dfrac{\lnot\lnot\phi}{\phi}\ \lnot\lnot_e$                                                                                                           |
| Bottom $\bot$                | $\dfrac{\phi \quad \lnot\phi}{\bot}\ \bot_i$                                       | $\dfrac{\bot}{\phi}\ \bot_e$                                                                                                                           |


Derived rules:

| rule name               | formulation                                                                         |
|-------------------------|-------------------------------------------------------------------------------------|
| modus tollens           | $\dfrac{\lnot\psi \quad \phi\to\psi}{\lnot\phi}\ \text{MT}$                         |
| proof by contradiction  | $\dfrac{\begin{bmatrix}\phi \\ \vdots \\ \bot\end{bmatrix}}{\lnot\phi}\ \text{PBC}$ |
| law of exccluded middle | $\dfrac{}{\phi \lor \lnot\phi}\ \text{LEM}$                                         |


## todo

- [ ] use exercises in _logic in computer science_ as tests to validate the correctness of this checker
- [ ] test whether the checker does reject typical invalid proofs
- [x] support referencing proved statements in parent blocks
- [x] implement a DSL for writing proofs
- [ ] enable named atomic propositions
- [ ] test proof parser

## license

Provided under the _GPL v3_ license.

## references

```plaintext
@book{huth2004logic,
  title={Logic in Computer Science: Modelling and reasoning about systems},
  author={Huth, Michael and Ryan, Mark},
  year={2004},
  publisher={Cambridge university press}
}
```
