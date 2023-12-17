-- rule:: pre-conditions -> post-condition -> Bool
module Rules
  ( conjI,
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

import Prop (Prop (And, Bottom, Impl, Not, Or))

-- conjunction introduction
-- p, q |- p /\ q
conjI :: Prop -> Prop -> Prop -> Bool
conjI p q (p' `And` q') = (p, q) == (p', q') || (q, p) == (p', q')
conjI _ _ _ = False

-- conjunction elimination
-- p /\ q |- p
-- p /\ q |- q
conjE :: Prop -> Prop -> Bool
conjE (p `And` q) r = r == p || r == q
conjE _ _ = False

-- disjunction introduction
-- p |- p /\ q
-- q |- p /\ q
disjI :: Prop -> Prop -> Bool
disjI r (p `Or` q) = r == p || r == q
disjI _ _ = False

-- disjunction elimination
-- p \/ q, p -> r, q -> r |- r
disjE :: Prop -> Prop -> Prop -> Prop -> Bool
disjE (p `Or` q) (p' `Impl` rp) (q' `Impl` rq) r = p == p' && q == q' && r == rp && r == rq
disjE _ _ _ _ = False

-- implication elimination
-- p, p -> q |- q
implE :: Prop -> Prop -> Prop -> Bool
implE p (p' `Impl` q') q = p == p' && q == q'
implE _ _ _ = False

-- negation introduction
-- p -> BOTTOM |- not p
negI :: Prop -> Prop -> Bool
negI (p `Impl` Bottom) (Not p') = p == p'
negI _ _ = False

-- bottom introduction
-- p, not p |- BOTTOM
botI :: Prop -> Prop -> Prop -> Bool
botI p (Not p') Bottom = p == p'
botI _ _ _ = False

-- bottom elimination
-- BOTTOM |- p
botE Bottom _ = True
botE _ _ = False

-- double negation introduction
-- p |- not not p
negnegI :: Prop -> Prop -> Bool
negnegI p (Not (Not p')) = p == p'
negnegI _ _ = False

-- double negation elimination
-- not not p |- p
negnegE :: Prop -> Prop -> Bool
negnegE (Not (Not p)) p' = p == p'
negnegE _ _ = False
