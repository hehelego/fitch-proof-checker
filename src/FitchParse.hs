module FitchParse where

import Control.Applicative (Alternative (empty, many, some, (<|>)))
import Control.Monad (guard)
import Data.Functor (($>), (<$), (<$>))
import Proof (Proof (..), Rule (..), Step (..), StepRef (..))
import Prop (Prop (..))

newtype Parser a = Parser {runParser :: String -> Either String (a, String)}

instance Monad Parser where
  (Parser p) >>= q =
    Parser $ \s ->
      case p s of
        Left e -> Left e
        Right (x, s') -> runParser (q x) s'

instance MonadFail Parser where
  fail e = Parser (const (Left e))

instance Applicative Parser where
  pure x = Parser (\s -> Right (x, s))
  (Parser pf) <*> q = Parser $ \s -> case pf s of
    Left e -> Left e
    Right (f, s') -> runParser (f <$> q) s'

instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (mapFst f) . p)
    where
      mapFst f (x, y) = (f x, y)

instance Alternative Parser where
  (Parser p) <|> (Parser q) =
    Parser $ \s -> case p s of
      Left _ -> q s
      Right x -> Right x
  many p = some p <|> return []
  some p = (:) <$> p <*> many p
  empty = fail "empty result"

anyChar :: Parser Char
anyChar = Parser $ \s -> case s of
  "" -> Left "no character can be extracted from an empty string"
  (c : s) -> Right (c, s)

condChar :: (Char -> Bool) -> Parser Char
condChar cond = do
  c <- anyChar
  if cond c
    then return c
    else fail "unexpected character"

char :: Char -> Parser Char
char c = condChar (== c)

newline :: Parser ()
newline = symbol "\\n"

string :: String -> Parser String
string "" = pure ""
string (c : s) = (:) <$> char c <*> string s

space :: Parser ()
space = condChar (== ' ') $> ()

spaces :: Parser ()
spaces = many space $> ()

token :: Parser a -> Parser a
token p = spaces *> p

symbol :: String -> Parser ()
symbol s = (spaces *> string s $> ()) <|> fail ("expected symbol: " ++ s)

oneOf :: [Char] -> Parser Char
oneOf cs = condChar (`elem` cs)

noneOf :: [Char] -> Parser Char
noneOf cs = condChar (`notElem` cs)

between :: Parser a -> Parser c -> Parser b -> Parser b
between l r p = l *> p <* r

intP :: Parser Int
intP = read <$> some (oneOf "0123456789")

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
chainl1 p op = p >>= rest
  where
    rest x =
      do
        f <- op
        y <- p
        rest $ f x y
        <|> pure x

propP :: Parser Prop
propP = chainl1 term impl
  where
    term = chainl1 factor andor
    factor = between (symbol "(") (symbol ")") propP <|> notP <|> atom
    impl = symbol "IMPL" $> Impl
    andor = symbol "AND" $> And <|> symbol "OR" $> Or
    atom = token $ bottomP <|> atomP

atomP :: Parser Prop
atomP = Atom <$> surrounded intP
  where
    surrounded = between (char '[') (char ']')

bottomP :: Parser Prop
bottomP = Bottom <$ symbol "BOTTOM"

notP :: Parser Prop
notP = Not <$> (symbol "NOT" *> propP)

refP :: Parser StepRef
refP = token intP

ruleP =
  symbol "Intr_Conj" *> (ConjI <$> refP <*> refP)
    <|> symbol "Elim_Conj" *> (ConjE <$> refP)
    <|> symbol "Intr_Disj" *> (DisjI <$> refP)
    <|> symbol "Elim_Disj" *> (DisjE <$> refP <*> refP <*> refP)
    <|> symbol "Elim_Impl" *> (ImplE <$> refP <*> refP)
    <|> symbol "Intr_Neg" *> (NegI <$> refP)
    <|> symbol "Intr_Bot" *> (BotI <$> refP <*> refP)
    <|> symbol "Elim_Bot" *> (BotE <$> refP)
    <|> symbol "Intr_Negneg" *> (NegNegI <$> refP)
    <|> symbol "Elim_Negneg" *> (NegNegE <$> refP)
    <|> fail "unrecognized inference rule"

proofP :: Parser Proof
proofP = many $ step <* eol
  where
    step = premiseP <|> deriveP <|> asumeP <|> endAssumeP
    premiseP = AddPremise <$> (symbol "Premise" *> token propP)
    deriveP = ApplyRule <$> (symbol "Derive" *> token propP) <*> ruleP
    asumeP = IntrAsump <$> (symbol "Intr-Asump" *> token propP)
    endAssumeP = ElimAsump <$> (symbol "Elim-Asump" *> token propP)

eol = spaces <* char '\n'
