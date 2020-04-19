-- |

module JsonParser where

data Json = JsonNumber Int | JsonString String | JsonList [Json] | JsonObject [(String, Json)]
