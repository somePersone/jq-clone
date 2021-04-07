module Jq.JParser where

import Parsing.Parsing
import Jq.Json
import Jq.SParser
import Jq.KParser   
import qualified Data.Map as Map

parseJNull :: Parser JSON
parseJNull = do _ <- string "null"
                return JNull

parseJBool :: Parser JSON
parseJBool = do s <- string "true" <|> string "false"
                return (JBool (s == "true"))

parseJNum :: Parser JSON
parseJNum = do s <- char '-' <|> char '+' <|> return '+'
               xs <- some digit
               d <- parseDecimal <|> return "0"
               e <- parseENum <|> return 0
               return (
                   JNum (
                       read (xs ++ "." ++ d) 
                       * (10^e) * (if s == '+' then 1 else -1)))

parseDecimal :: Parser String 
parseDecimal = do _ <- char '.'
                  some digit 

parseENum :: Parser Int
parseENum = do _ <- char 'E'
               s <- char '-' <|> char '+'
               i <- some digit
               return (case s of 
                        '+' -> read i
                        '-' -> -(read i)
                        _ -> error "Not valid sign")          

parseJString :: Parser JSON
parseJString = do JString <$> parseString

parseJArray :: Parser JSON
parseJArray = do _ <- symbol "["
                 a <- arrayElem <|> return []
                 _ <- symbol "]"
                 return (JArray a)


arrayElem :: Parser [JSON]
arrayElem =  do j <- parseJSON
                js <- many (do _ <- symbol ","
                               parseJSON)
                return (j:js)


parseJObject :: Parser JSON
parseJObject = do _ <- symbol "{"
                  kvs <- keyValues <|> return []
                  _ <- symbol "}"
                  return (JObject (Map.fromList kvs))

keyValues :: Parser [(String, JSON)]
keyValues =  do kv <- keyValue
                kvs <- many (do _ <- symbol ","
                                keyValue)
                return (kv:kvs)


keyValue :: Parser (String, JSON)
keyValue = do n <- parseString <|> parseKey
              _ <- symbol ":"
              v <- parseJSON
              return (n, v)


parseJSON :: Parser JSON
parseJSON = token $ (parseJNull <|> parseJBool <|> parseJNum <|> parseJString <|> parseJArray <|> parseJObject)
