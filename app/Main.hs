module Main where

import JsonParser
import System.Environment

main :: IO ()
main = do
  [fp] <- getArgs
  json <- parseFile fp
  putStrLn $ show json


parseFile :: FilePath -> IO (Maybe (String, JsonValue))
parseFile fp = do
  input <- readFile fp
  return (runParser jsonValue input)
