module Rule where

data Rule
  = Premise -- use premise
  | Assumption -- use assumption
  | ConjI -- conjunction introduction
  | ConjE -- conjunction elimination
  | DisjI -- disjunction introduction
  | DisjE -- disjunction elimination
  | ImplI -- implication introduction
  | ImplE -- implication elimination
  | NegI -- negation introduction
  | NegE -- negation elimination
  | BotE -- bottom elimination
  | NegNegI -- double negation introduction
  | NegNegE -- double negation elimination
  deriving (Show, Eq, Ord)
