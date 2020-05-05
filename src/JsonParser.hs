-- |

module JsonParser
  ( charP
  , stringP
  , numberP
  , intP
  , predP
  , ws
  , jsonValue
  , Parser(runParser)
  , JsonValue(..)
  )
where
import           Data.Bifunctor                 ( second )
import           Data.Char                      ( isDigit
                                                , isSpace
                                                , isHexDigit
                                                , chr
                                                , intToDigit
                                                )
import           Numeric                        ( readHex )
import           Control.Applicative            ( Alternative(..)
                                                , optional
                                                )
import           Data.Functor                   ( ($>) )
import           Data.Tuple                     ( swap )
import           Data.Maybe                     ( fromMaybe )
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonNumber Int
  | JsonString String
  | JsonList [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)


newtype Parser a = Parser
  { runParser :: String -> Maybe (String, a)
  }

instance Functor Parser where
  fmap f (Parser p) = Parser $ fmap (second f) . p

instance Applicative Parser where
  pure v = Parser $ \input -> Just (input, v)
  (Parser ff) <*> (Parser af) = Parser $ \input -> do
    (input' , f) <- ff input
    (input'', a) <- af input'
    return (input'', f a)

instance Alternative Parser where
  empty = Parser $ const Nothing
  (Parser p1) <|> (Parser p2) = Parser $ \input -> p1 input <|> p2 input

parseFile :: FilePath -> IO (Maybe (String, JsonValue))
parseFile fp = do
  input <- readFile fp
  print input
  putStrLn ""
  return (runParser jsonValue input)

jsonValue :: Parser JsonValue
jsonValue =
  surroundWs
    $   jsonNull
    <|> jsonBool
    <|> jsonNumber
    <|> jsonString
    <|> jsonList
    <|> jsonObject

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charP '{' *> jsonSequence jsonPair <* charP '}')

jsonPair :: Parser (String, JsonValue)
jsonPair = (,) <$> (surroundWs stringLiteral <* charP ':') <*> jsonValue

jsonList :: Parser JsonValue
jsonList = JsonList <$> (charP '[' *> jsonSequence jsonValue <* charP ']')

jsonSequence :: Parser a -> Parser [a]
jsonSequence p = sepBy (charP ',') p <|> ws $> []

jsonNumber :: Parser JsonValue
jsonNumber = fmap JsonNumber intP

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonBool :: Parser JsonValue
jsonBool =
  stringP "true" $> JsonBool True <|> stringP "false" $> JsonBool False

jsonNull :: Parser JsonValue
jsonNull = stringP "null" $> JsonNull

surroundWs :: Parser a -> Parser a
surroundWs p = ws *> p <* ws

ws :: Parser String
ws = many (predP isSpace)

-- TODO handle escape sequences
stringLiteral :: Parser String
stringLiteral = charP '"' *> many (escapeChars <|> predP (/= '"')) <* charP '"'

escapeChars :: Parser Char
escapeChars =
  ('"' <$ stringP "\\\"")
    <|> ('\\' <$ stringP "\\\\")
    <|> ('\b' <$ stringP "\\b")
    <|> ('\f' <$ stringP "\\f")
    <|> ('\n' <$ stringP "\\n")
    <|> ('\r' <$ stringP "\\r")
    <|> ('\t' <$ stringP "\\t")
    <|> (stringP "\\u" *> unicodeLiteral)

unicodeLiteral :: Parser Char
unicodeLiteral =
  chr . fst . head . readHex <$> sequenceA (replicate 4 (predP isHexDigit))

intP :: Parser Int
intP = read <$> numberP <|> (stringP "-" *> (negate <$> intP))

numberP :: Parser String
numberP = some (predP isDigit)

doubleP :: Parser Double
doubleP = read . concat <$> sequenceA
  [optionalP minus, int, optionalP frac, optionalP exp']
 where
  int       = (:) <$> onetonine <*> many digits <|> zero
  frac      = (:) <$> charP '.' <*> some digits
  exp'      = concat <$> sequenceA [e, optionalP sign, many digits]
  e         = stringP "e" <|> stringP "E"
  sign      = minus <|> plus
  minus     = stringP "-"
  plus      = stringP "+"
  onetonine = predP (`elem` map intToDigit [1 .. 9])
  zero      = stringP "0"
  digits    = predP isDigit


stringP :: String -> Parser String
stringP = traverse charP

charP :: Char -> Parser Char
charP c = predP (== c)

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element)

predP :: (Char -> Bool) -> Parser Char
predP f = Parser parseIfPred
 where
  parseIfPred :: String -> Maybe (String, Char)
  parseIfPred (x : xs) | f x = Just (xs, x)
  parseIfPred _              = Nothing

spanP :: (Char -> Bool) -> Parser String
spanP cond = Parser $ Just . swap . span cond

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> case p input of
  Just (_, []) -> Nothing
  x            -> x

optionalP :: Parser String -> Parser String
optionalP p = fromMaybe "" <$> optional p
