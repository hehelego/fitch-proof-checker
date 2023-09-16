module Prop  where

newtype AtomId = AtomId Int deriving (Show, Eq, Ord)

data Prop
  = Bottom
  | Atom AtomId
  | Not Prop
  | And Prop Prop
  | Or Prop Prop
  | Impl Prop Prop
  deriving (Show, Eq, Ord)
