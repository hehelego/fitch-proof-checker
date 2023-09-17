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


- conjunction introduction:  
    $$\dfrac{\phi \quad \psi}{\phi \land \psi}\ \land_i$$  
- conjunction elimination:  
    $$\dfrac{\phi \land \psi}{\phi}\ \land_e \qquad \dfrac{\phi \land \psi}{\psi}\ \land_e$$  
- disjunction introduction:  
    $$\dfrac{\phi}{\phi \lor \psi}\ \lor_i \qquad \dfrac{\psi}{\phi \lor \psi}\ \lor_i$$  
- disjunction elimination:  
    $$\dfrac{\phi \lor \psi\quad \begin{bmatrix}\phi \\ \vdots \\ \chi\end{bmatrix}\quad \begin{bmatrix}\psi \\ \vdots \\ \chi\end{bmatrix}}{\chi}\ \lor_e$$  
- implication introduction:  
    $$\dfrac{\begin{bmatrix}\phi \\ \vdots \\ \psi\end{bmatrix}}{\phi \to \psi}\ \to_i$$  
- implication elimination (latin: modus ponens):  
    $$\dfrac{\phi \quad \phi\to\psi}{\psi}\ \to_e$$
- negation introduction:  
    $$\dfrac{\begin{bmatrix}\phi \\ \vdots \\ \bot\end{bmatrix}}{\lnot\phi}\ \lnot_i$$
- negation elimination:  
    $$\dfrac{\phi \quad \lnot\phi}{\bot}\ \lnot_e$$
- bottom elimination:  
    $$\dfrac{\bot}{\phi}\ \bot_e$$
- double negation elimination:  
    $$\dfrac{\lnot\lnot\phi}{\phi}\ \lnot\lnot_e$$

Derived rules:

- modus tollens:  
    $$\dfrac{\lnot\psi \quad \phi\to\psi}{\lnot\phi}\ \text{MT}$$
- double negation introduction:  
    $$\dfrac{\phi}{\lnot\lnot\phi}\ \lnot\lnot_i$$
- proof by contradiction:  
    $$\dfrac{\begin{bmatrix}\phi \\ \vdots \\ \bot\end{bmatrix}}{\lnot\phi}\ \text{PBC}$$
- law of excluded middle:  
    $$\dfrac{}{\phi \lor \lnot\phi}\ \text{LEM}$$



## todo

- [ ] use exercises in _logic in computer science` as tests to validate the correctness of this checker
- [ ] test whether the checker dose reject typical invalid proofs
- [ ] support referencing proved statements in parent blocks
- [ ] implement a DSL for writing proofs

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
