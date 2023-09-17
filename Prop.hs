module Prop where

type AtomId = Int

data Prop
    = Bottom
    | Atom AtomId
    | Not Prop
    | And Prop Prop
    | Or Prop Prop
    | Impl Prop Prop
    deriving (Show, Eq, Ord)
