module Prop where

type AtomId = Int

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
