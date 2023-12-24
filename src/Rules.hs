-- |
-- A rule determines whether a set of premises (possibly empty) entails a conclusion.
--
-- The @IntrAsump p@ and @ElimAsump q@ proof steps automatically derives \(p\to q\) from the hypothetical reasoning.
-- Therefore, an Implication Introduction rule is not necessary.
module Rules
  ( lem,
    conjI,
    conjE,
    disjI,
    disjE,
    implE,
    negI,
    botI,
    botE,
    negnegI,
    negnegE,
  )
where

import Proof
  ( Arity0Rule (Arity0Rule),
    Arity1Rule (Arity1Rule),
    Arity2Rule (Arity2Rule),
    Arity3Rule (Arity3Rule),
    RuleApp (A0, A1, A2, A3),
  )
import Prop (Prop (And, Bottom, Impl, Not, Or))

-- | law of excluded middle.
-- \[
-- \vdash p \lor \lnot p
-- \]
lem = A0 $ Arity0Rule "LEM" lem'
  where
    lem' (p `Or` q) = q == Not p
    lem' _ = False

-- | conjunction introduction.
-- \[
-- p, q \vdash p \land q
-- \]
conjI = A2 $ Arity2Rule "/\\ i" conjI'
  where
    conjI' p q (p' `And` q') = (p, q) == (p', q') || (q, p) == (p', q')
    conjI' _ _ _ = False

-- | conjunction elimination.
-- \[
-- p \land q \vdash p
-- \quad
-- p \land q \vdash q
-- \]
conjE = A1 $ Arity1Rule "/\\ e" conjE'
  where
    conjE' (p `And` q) r = r == p || r == q
    conjE' _ _ = False

-- | disjunction introduction.
-- \[
-- p \vdash p \lor q
-- \land
-- q \vdash p \lor q
-- \]
disjI = A1 $ Arity1Rule "\\/ i" disjI'
  where
    disjI' r (p `Or` q) = r == p || r == q
    disjI' _ _ = False

-- | disjunction elimination.
-- \[
-- p\lor q, p\to r, q\to r \vdash r
-- \]
disjE = A3 $ Arity3Rule "\\/ e" disjE'
  where
    disjE' (p `Or` q) (p' `Impl` rp) (q' `Impl` rq) r = p == p' && q == q' && r == rp && r == rq
    disjE' _ _ _ _ = False

-- | implication elimination a.k.a. modus ponens (MP).
-- \[
-- p, p\to q \vdash q
-- \]
implE = A2 $ Arity2Rule "-> e" implE'
  where
    implE' p (p' `Impl` q') q = p == p' && q == q'
    implE' _ _ _ = False

-- | negation introduction.
-- \[
-- p\to \bot \vdash \lnot p
-- \]
negI = A1 $ Arity1Rule "~ i" negI'
  where
    negI' (p `Impl` Bottom) (Not p') = p == p'
    negI' _ _ = False

-- | bottom introduction.
-- \[
-- p,\lnot p \vdash \bot
-- \]
botI = A2 $ Arity2Rule "⊥ i" botI'
  where
    botI' p (Not p') Bottom = p == p'
    botI' _ _ _ = False

-- | bottom elimination.
-- \[
-- \bot \vdash p
-- \]
botE = A1 $ Arity1Rule "⊥ e" botE'
  where
    botE' Bottom _ = True
    botE' _ _ = False

-- | double negation introduction.
-- \[
-- p \vdash \lnot\lnot p
-- \]
negnegI = A1 $ Arity1Rule "~~ i" negnegI'
  where
    negnegI' p (Not (Not p')) = p == p'
    negnegI' _ _ = False

-- | double negation elimination.
-- \[
-- \lnot\lnot p \vdash p
-- \]
negnegE = A1 $ Arity1Rule "~~ e" negnegE'
  where
    negnegE' (Not (Not p)) p' = p == p'
    negnegE' _ _ = False
