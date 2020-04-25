-- |

module JsonParser where
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

