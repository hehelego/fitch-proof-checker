module Tableau where

import Proof
import Prop

-- Determine if a set of formula is satisfiable.
-- Return:
-- unsat (Left). A proof ⊢ p1 /\ p2 /\ p3 ... pn -> ⊥
-- sat (Right). An assignment v where evaluate [p1,p2 .. pn] v = True
tableau :: [Prop] -> Either Proof Assignment
tableau = undefined

-- Apply Tableaux method to prove {p1, p2 .. pn} ⊢ q
tableauProof :: [Prop] -> Prop -> Maybe Proof
tableauProof = undefined
