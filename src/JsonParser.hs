-- |

module JsonParser
  ( charP
  , stringP
  , jsonNull
  , jsonBool
  , Parser(runParser)
  , JsonValue(..)
  )
where
import           Data.Bifunctor                 ( second )
import           Data.Char                      ( isNumber )
import           Control.Applicative            ( Alternative(..) )
import Data.Functor (($>))
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

jsonValue :: Parser JsonValue
jsonValue = undefined

jsonNull :: Parser JsonValue
jsonNull = stringP "null" *> pure JsonNull

jsonBool :: Parser JsonValue
jsonBool =  stringP "true" $> JsonBool True <|> stringP "false" $> JsonBool False

jsonNumber :: Parser JsonValue
jsonNumber = undefined

jsonString :: Parser JsonValue
jsonString = undefined

jsonList :: Parser JsonValue
jsonList = undefined

jsonObject :: Parser JsonValue
jsonObject = undefined

charP :: Char -> Parser Char
charP = Parser . runCharP
 where
  runCharP :: Char -> String -> Maybe (String, Char)
  runCharP c (x : xs) | x == c = Just (xs, c)
  runCharP _ _                 = Nothing

stringP :: String -> Parser String
stringP = traverse charP
