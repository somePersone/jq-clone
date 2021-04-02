module Jq.SParser where

import Parsing.Parsing ( Parser, char, item, Alternative((<|>)) )

parseString :: Parser String
parseString = do _ <- char '\"'
                 pString

pString :: Parser String
pString = escape <|> closeString <|> basicChar 
                 
escape :: Parser String 
escape = do _ <- char '\\'
            x <- item 
            xs <- pString
            return (x:xs)   

closeString :: Parser String 
closeString = do _ <- char '\"'
                 return []  

basicChar :: Parser String 
basicChar = do x <- item 
               xs <- pString
               return (x:xs)