module Jq.CParser where

import Parsing.Parsing
import Jq.Filters
import Jq.SParser
import Jq.KParser
import Jq.JParser

parseIdentity :: Parser Filter
parseIdentity = do
  _ <- token . char $ '.'
  return Identity

parseParenthesis :: Parser Filter
parseParenthesis = do
  _ <- symbol "("
  f <- parseFilter 
  _ <- symbol ")"
  return (Parenthesis f)

parseArrayIndex :: Parser Filter
parseArrayIndex = do
  _ <- token . char $ '.'
  _ <- char '['
  n <- int 
  _ <- char ']'
  return (ArrayIndex n)

parseArraySlice :: Parser Filter
parseArraySlice = do
  _ <- char '.'
  _ <- char '['
  a <- fromNtOM <|> toM <|> fromN
  _ <- char ']'
  return a
    where
      toM = do
        _ <- char ':'
        m <- int
        return (ArraySlice Nothing (Just m))
      fromN = do
        n <- int 
        _ <- char ':'
        return (ArraySlice (Just n) Nothing )
      fromNtOM = do
        n <- int 
        _ <- char ':'
        m <- int
        return (ArraySlice (Just n) (Just m))


parseItterateAll :: Parser Filter
parseItterateAll = do
  _ <- symbol ".[]"
  return ItterateAll

parseItterateSelect :: Parser Filter
parseItterateSelect =  do 
  _ <- symbol ".["
  n <- int
  ns <- many (do
    _ <- symbol ","
    int)
  _ <- symbol "]"  
  return (ItterateSelect (n:ns))

parseObjectIndexs :: Parser Filter 
parseObjectIndexs = do
  o <- parseObjectIndex
  os <- many parseObjectIndex
  return (applyPipe o os)
    where 
      applyPipe :: Filter -> [Filter] -> Filter
      applyPipe o [] = o
      applyPipe o (a:os) = Pipe o (applyPipe a os)


parseObjectIndex :: Parser Filter
parseObjectIndex = do
  _ <- char '.'
  i <- parseKey <|> parseString <|> (do 
    _ <- char '['
    s <- parseString
    _ <- char ']'
    return s)
  p <- string "[]" <|> pure "_"
  q <- char '?' <|> pure '_'
  oi <- if q == '?' then return (Optional (ObjectIndex i)) else return (ObjectIndex i)
  if p == "[]" then return (Pipe oi ItterateAll) else return oi
  


parseOptional :: Parser Filter
parseOptional = do 
    j <- parseItterateAll <|> parseItterateSelect <|> parseArrayIndex <|> parseArraySlice
    _ <- char '?'
    return (Optional j)
    
parseValue :: Parser Filter
parseValue = do Value <$> (parseJBool <|> parseJNull <|> parseJNum <|> parseJString)

parseArray :: Parser Filter
parseArray =
    (Value <$> parseJArray) <|> 
    (do _ <- char '['
        f <- parseFilter
        _ <- char ']'
        return (Array f))
    

 

parseObject :: Parser Filter
parseObject = 
  (Value <$> parseJObject) 
  <|>
  (do
      _ <- char '{'
      os <- kvs
      _ <- token . char $ '}'
      return (Object os))
  where
    kv :: Parser ObjectItem
    kv = do n <- parseString <|> parseKey 
            _ <- symbol ":"
            StringValue n <$> fill 
    name :: Parser ObjectItem
    name = do n <- parseString <|> parseKey
              return (StringValue n (ObjectIndex n))
    filterName :: Parser ObjectItem  
    filterName = do p <- parseParenthesis
                    _ <- symbol ":" 
                    ValueValue p <$> fill
    kvs :: Parser [ObjectItem]      
    kvs = do o <- kv <|> name <|> filterName
             os <- many (do _ <- symbol ","
                            kv <|> name <|> filterName)
             return (o:os)               

parseRecursiveDecent :: Parser Filter
parseRecursiveDecent = do
  _ <- string ".."
  return RecursiveDecent    

parseNot :: Parser Filter
parseNot = do
  _ <- symbol "not"
  return Not


parseOperation :: Filter -> Parser Filter   
parseOperation f = do
  s <-  (token . char $ '+') 
      <|> (token . char $ '-')
      <|> (token . char $ '*')
      <|> (token . char $ '/')
  let o = case s of
            '+' -> Add 
            '-' -> Sub
            '*' -> Mul
            '/' -> Div
  Opperation o f <$> fill  


parseComparisons :: Filter -> Parser Filter   
parseComparisons f = do
  s <-  (symbol "==") <|> (symbol "!=" ) <|> (symbol "<=" ) <|> (symbol ">=" ) <|> (symbol "<" )  <|> (symbol ">" ) <|> (symbol "and") <|> (symbol "or")
  let c = case s of
            "==" -> Equal 
	    "!=" -> NotEqual
	    "<" -> LessThan
	    "<=" -> LessThanEqual
	    ">" -> GreaterThan
	    ">=" -> GreaterThanEqual
	    "and" -> And
	    "or" -> Or
  Comparisons c f <$> fill  

fill :: Parser Filter
fill = parseRecursiveDecent  
      <|>parseOptional
      <|> parseItterateAll 
      <|> parseItterateSelect 
      <|> parseArrayIndex 
      <|> parseArraySlice 
      <|> parseParenthesis
      <|> parseObjectIndexs
      <|> parseIdentity
      <|> parseArray
      <|> parseValue
      <|> parseObject 
      <|> parseRecursiveDecent
      <|> parseNot
      >>= \f ->
        parseOperation f <|> parseComparisons f <|> return f

commas :: Parser Filter
commas = do
  f <- fill
  c <- symbol "," <|> return "_"
  if c == "," then Comma f <$> commas else return f


parseIfThenElse :: Parser Filter
parseIfThenElse = do
  _ <- symbol "if"
  i <- parseFilter  
  _ <- symbol "then"
  t <- parseFilter
  _ <- symbol "else"
  e <- parseFilter
  _ <- symbol "end"
  return (IfThenElse i t e)

parseFilter :: Parser Filter
parseFilter = parseIfThenElse <|> do
  c <- commas
  p <- symbol "|" <|> return "_"
  if p == "|" then Pipe c <$> parseFilter else return c




parseConfig :: [String] -> Either String Config
parseConfig s = case s of
  [] -> Left "No filters provided"
  h : _ ->
    case parse parseFilter h of
      [(v, out)] -> case out of
        [] -> Right . ConfigC $ v
        _ -> Left $ "Compilation error, leftover: " ++ out
      e -> Left $ "Compilation error: " ++ show e
