module Prop where

type AtomId = Int

type Assignment = [(AtomId, Bool)]

data Prop
  = Bottom
  | Atom AtomId
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Impl Prop Prop
  deriving (Eq, Ord)

instance Show Prop where
  show Bottom = "⊥"
  show (Atom i) = "{" ++ show i ++ "}"
  show (Not p) = "¬" ++ show p
  show (p `And` q) = "(" ++ show p ++ "∧" ++ show q ++ ")"
  show (p `Or` q) = "(" ++ show p ++ "∨" ++ show q ++ ")"
  show (p `Impl` q) = "(" ++ show p ++ "→" ++ show q ++ ")"

evaluate :: Prop -> Assignment -> Bool
evaluate Bottom _ = False
evaluate (Atom i) v = case lookup i v of
  Just x -> x
  Nothing -> error "Atomic proposition "
evaluate (Not p) v = not $ evaluate p v
evaluate (p `And` q) v = evaluate p v && evaluate q v
evaluate (p `Or` q) v = evaluate p v || evaluate q v
evaluate (p `Impl` q) v = not (evaluate p v) || evaluate q v
