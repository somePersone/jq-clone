module Jq.SParser where

import Parsing.Parsing 

parseString :: Parser String
parseString = do _ <- char '\"'
                 pString

pString :: Parser String
pString = unicode <|> escape <|> closeString <|> basicChar 
                 
unicode :: Parser String
unicode = do _ <- char '\\'
	     _ <- char 'u'
	     v <- pure hexToUnicode <*> alphanum <*> alphanum <*> alphanum <*> alphanum
	     xs <- pString
	     return(v: xs)
	     

digitToInt :: Char -> Int
digitToInt '0' = 0
digitToInt '1' = 1
digitToInt '2' = 2
digitToInt '3' = 3
digitToInt '4' = 4
digitToInt '5' = 5
digitToInt '6' = 6
digitToInt '7' = 7
digitToInt '8' = 8
digitToInt '9' = 9
digitToInt 'A' = 10
digitToInt 'a' = 10
digitToInt 'B' = 11
digitToInt 'b' = 11
digitToInt 'C' = 12
digitToInt 'c' = 12
digitToInt 'D' = 13
digitToInt 'd' = 13
digitToInt 'E' = 14
digitToInt 'e' = 14
digitToInt 'F' = 15
digitToInt 'f' = 15
digitToInt 'G' = 16
digitToInt 'g' = 16

hexToInt :: Char -> Char -> Char -> Char -> Int
hexToInt a b c d = (digitToInt a * 16^3) + (digitToInt b * 16^2) + (digitToInt c * 16) + digitToInt d

hexToUnicode :: Char -> Char -> Char -> Char -> Char
hexToUnicode = (((toEnum .) .) .) .  hexToInt 

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
