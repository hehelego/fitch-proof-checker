module StackMultiSet where

import Data.List (uncons)
import qualified Data.Map as Map

data StackMultiSet a = StackMultiSet (Map.Map a Int) [a]
  deriving (Show, Eq)

empty = StackMultiSet Map.empty []

push (StackMultiSet map stk) x =
  let map' = Map.insertWith (+) x 1 map
      stk' = (x : stk)
   in StackMultiSet map' stk'

pop (StackMultiSet map stk) = case stk of
  (x : stk') ->
    let map' = Map.update (\c -> if c > 1 then Just $ c - 1 else Nothing) x map
     in Right (StackMultiSet map' stk', x)
  [] -> Left "Nothing can be poped."

member (StackMultiSet map _) x = Map.member x map

null (StackMultiSet map _) = Map.null map
