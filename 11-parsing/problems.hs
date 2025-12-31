{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}

import Control.Applicative (Alternative (..))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isSpace)

newtype ParseError = ParseError {getParseError :: String}
  deriving Show

-- 1. Нека е даден следният АТД, представящ парсер:

newtype Parser a = Parser {runParser :: String -> Either ParseError (a, String)}

-- Да се създадат инстанции на Functor, Applicative и Monad за Parser.

instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (Parser parse) = Parser $ \str -> case parse str of
                                            Left err -> Left err
                                            Right (a, rest) -> Right (f a, rest)

instance Applicative Parser where
    pure :: a -> Parser a
    pure x = Parser $ \str -> Right (x, str)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    Parser p1 <*> Parser p2 =
        Parser $ \str -> case p1 str of
            Left err -> Left err
            Right (f, str') -> case p2 str' of
                Left err -> Left err
                Right (a, str'') -> Right (f a, str'')

instance Monad Parser where
    return :: a -> Parser a
    return = pure

    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    Parser p1 >>= f =
        Parser $ \str -> case p1 str of
            Left err -> Left err
            Right (a, str') -> runParser (f a) str'

instance Alternative Parser where
    empty :: Parser a
    empty = Parser $ \_ -> Left (ParseError "unknown error")

    (<|>) :: Parser a -> Parser a -> Parser a
    Parser p1 <|> Parser p2 =
        Parser $ \str -> case p1 str of
            Left _ -> p2 str
            Right res -> Right res

-- 02. Нека е дадена следната граматика:

-- term ::= term '|'  term
--        | term '&'  term
--        | term '=>' term
--        | '!' term
--        | '(' term ')'
--        | variable

-- variable ::= [A-Z][a-zA-Z0-9_]*

-- Да се създаде парсер, който parse-ва низ от горния вид до някакво вътрешно представяне. Може да приемете, че всички оператори имат еднакви приоритет и асоциативност.

data Term = Var String
    | Or Term Term
    | And Term Term
    | Implies Term Term
    | Not Term
    deriving Show

item :: Parser Char
item = Parser $ \s -> case s of
    [] -> Left $ ParseError "empty stream"
    h : t -> pure (h, t)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \str -> case str of
    h : t
        | p h -> pure (h, t)
        | otherwise -> Left $ ParseError $ "unexpected character '" ++ [h] ++ "'"
    [] -> Left $ ParseError "empty stream"

eof :: Parser ()
eof = Parser $ \str -> case str of
    [] -> pure ((), [])
    _ -> Left $ ParseError "still more input"

char :: Char -> Parser Char
char c = satisfy (== c)

lower :: Parser Char
lower = satisfy isAsciiLower

upper :: Parser Char
upper = satisfy isAsciiUpper

digit :: Parser Char
digit = satisfy isDigit

underscore :: Parser Char
underscore = char '_'

var :: Parser Term
var = Var <$> liftA2 (:) upper (many (lower <|> upper <|> digit <|> underscore))

token :: Parser a -> Parser a
token p = many (char ' ') *> p <* many (char ' ')

not' :: Parser Term
not' = char '!' >> Not <$> token (var <|> not' <|> paren)

paren :: Parser Term
paren = char '(' *> term <* char ')'

conj :: Term -> Parser Term
conj lhs = do
  _ <- char '&'
  And lhs <$> term

disj :: Term -> Parser Term
disj lhs = char '|' >> Or lhs <$> term

implies :: Term -> Parser Term
implies lhs = fmap (Implies lhs) $ char '=' >> char '>' >> term

lr :: [a -> Parser a] -> a -> Parser a
lr pfs x = foldr (<|>) (pure x) ps
 where
  ps = map ($ x) pfs

term :: Parser Term
term = do
  base <- token $ var <|> not' <|> paren
  lr [conj, disj, implies] base