module Prop
  ( AtomId,
    Assignment,
    Prop (Bottom, Atom, Not, And, Or, Impl),
    eval,
  )
where

type AtomId = Int

type Assignment = AtomId -> Bool

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

eval :: Prop -> Assignment -> Bool
eval Bottom _ = False
eval (Atom i) v = v i
eval (Not p) v = not $ eval p v
eval (p `And` q) v = eval p v && eval q v
eval (p `Or` q) v = eval p v || eval q v
eval (p `Impl` q) v = not (eval p v) || eval q v
