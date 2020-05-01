-- |

module JsonParser
  ( charP
  , stringP
  , numberP
  , intP
  , predP
  , jsonValue
  , Parser(runParser)
  , JsonValue(..)
  )
where
import           Data.Bifunctor                 ( second )
import           Data.Char                      ( isDigit )
import           Control.Applicative            ( Alternative(..) )
import           Data.Functor                   ( ($>) )
import           Data.Tuple                     ( swap )
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
jsonValue = jsonNull <|> jsonBool <|> jsonNumber

jsonObject :: Parser JsonValue
jsonObject = undefined

jsonList :: Parser JsonValue
jsonList = undefined

jsonNumber :: Parser JsonValue
jsonNumber = fmap JsonNumber intP

jsonString :: Parser JsonValue
jsonString = undefined

jsonBool :: Parser JsonValue
jsonBool =
  stringP "true" $> JsonBool True <|> stringP "false" $> JsonBool False

jsonNull :: Parser JsonValue
jsonNull = stringP "null" $> JsonNull

intP :: Parser Int
intP = read <$> numberP <|> (stringP "-" *> fmap negate intP)

numberP :: Parser String
numberP = notNull (many (predP isDigit))

stringP :: String -> Parser String
stringP = traverse charP

charP :: Char -> Parser Char
charP c = predP (== c)

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
