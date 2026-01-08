{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
import Control.Applicative
import Data.Char (isDigit, isSpace)

data ParseError = EOF | UnexpectedCharacter Char | UnknownError
  deriving (Show)

newtype Parser a = Parser {runParser :: String -> Either ParseError (a, String)}

instance Functor Parser where
  fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (Parser p) =
    Parser $ \s -> case p s of
      Left err -> Left err
      Right (a, rest) -> Right (f a, rest)

instance Applicative Parser where
  pure :: a -> Parser a
  pure x = Parser $ \s -> Right (x, s)

  (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  Parser p1 <*> Parser p2 =
    Parser $ \s -> case p1 s of
      Left err -> Left err
      Right (f, s') -> case p2 s' of
        Left err -> Left err
        Right (x, s'') -> Right (f x, s'')

instance Monad Parser where
  return :: a -> Parser a
  return = pure

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  Parser p >>= f =
    Parser $ \s -> case p s of
      Left err -> Left err
      Right (x, s') -> runParser (f x) s'

instance Alternative Parser where
  empty :: Parser a
  empty = Parser $ \_ -> Left UnknownError

  (<|>) :: Parser a -> Parser a -> Parser a
  Parser p1 <|> Parser p2 =
    Parser $ \s -> case p1 s of
      Left _ -> p2 s
      Right res -> Right res

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  [] -> Left EOF
  (c : cs) -> if p c then Right (c, cs) else Left $ UnexpectedCharacter c

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
string = traverse char

digit :: Parser Char
digit = satisfy isDigit

integer :: Parser String
integer = some digit

whitespace :: Parser String
whitespace = many (satisfy isSpace)

separateBy :: Parser a -> Parser b -> Parser [b]
separateBy separator element = liftA2 (:) element (many $ separator *> element) <|> pure []

data Json
  = JsonNull
  | JsonBool Bool
  | JsonNumber Double
  | JsonString String
  | JsonArray [Json]
  | JsonObject [(String, Json)]
  deriving (Show)

jsonNull :: Parser Json
jsonNull = JsonNull <$ string "null"

jsonBool :: Parser Json
jsonBool =
  JsonBool True <$ string "true"
    <|> JsonBool False <$ string "false"

jsonNumber :: Parser Json
jsonNumber = JsonNumber . read <$> numStr
  where
    numStr :: Parser String
    numStr = do
      sign <- optional (char '-')
      int <- integer
      frac <- optional (char '.' *> integer)
      let signStr = case sign of
            Just _ -> "-"
            Nothing -> ""
      case frac of
        Nothing -> pure (signStr ++ int)
        Just s -> pure (signStr ++ int ++ "." ++ s)

jsonString :: Parser Json
jsonString = JsonString <$> (quote *> many (satisfy (/= '"')) <* quote)
  where
    quote = char '"'

jsonArray =
  JsonArray
    <$> ( char '['
            *> whitespace
            *> separateBy (whitespace *> char ',' *> whitespace) jsonValue
            <* whitespace
            <* char ']'
        )

jsonValue :: Parser Json
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

jsonObjKVP :: Parser (String, Json)
jsonObjKVP = do
  keyJson <- jsonString
  whitespace
  char ':'
  whitespace
  val <- jsonValue
  case keyJson of
    JsonString key -> pure (key, val)
    _ -> empty -- i guess this should be unreachable? since JsonString always returns JsonString (if no error)

jsonObject =
  JsonObject
    <$> ( char '{'
            *> whitespace
            *> separateBy (whitespace *> char ',' *> whitespace) jsonObjKVP
            <* whitespace
            <* char '}'
        )

json :: String -> Either ParseError Json
json str = case runParser (whitespace *> jsonValue <* whitespace) str of
  Right (jsonVal, "") -> Right jsonVal
  Right (_, rest) -> Left UnknownError
  Left err -> Left err

parseJSONFile :: FilePath -> IO (Either ParseError Json)
parseJSONFile filename = do
  contents <- readFile filename

  return $ json contents

main :: IO ()
main = do
  result <- parseJSONFile "test.json"
  print result
