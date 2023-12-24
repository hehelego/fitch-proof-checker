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

eval :: Assignment -> Prop -> Bool
eval v prop = case prop of
  Bottom -> False
  Atom i -> v i
  Not p -> not $ e p
  p `And` q -> e p && e q
  p `Or` q -> e p || e q
  p `Impl` q -> not (e p) || e q
  where
    e = eval v
