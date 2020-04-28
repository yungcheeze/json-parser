-- |

module JsonParser
  ( charP
  , stringP
  , Parser(runParser)
  )
where
import Data.Bifunctor (second)
data JsonValue
  = JsonNull
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
    (input', f) <- ff input
    (input'', a) <- af input'
    return (input'', f a)

jsonValue :: Parser JsonValue
jsonValue = undefined

jsonNull :: Parser JsonValue
jsonNull = undefined

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
