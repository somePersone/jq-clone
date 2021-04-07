module Jq.Json where

import qualified Data.Map as Map

data JSON = JNull
          | JBool Bool  
          | JNum Float 
          | JString String 
          | JArray [JSON]
          | JObject (Map.Map String JSON)
      
instance Show JSON where
  show x = showWithIndent 0 x

showWithIndent :: Int -> JSON -> String
showWithIndent n JNull = "null" 
showWithIndent n (JBool True) = "true"
showWithIndent n (JBool False) = "false"
showWithIndent n (JNum x) = if fromInteger i == x then show i else show x
  where
    i = round x :: Integer
showWithIndent n (JString s) = "\"" ++ s ++ "\""
showWithIndent n (JArray []) = "[]"   
showWithIndent n (JArray a) = "[\n" ++ showArryElem (n + 1) a ++ indent n ++ "]"
showWithIndent n (JObject m) | Map.null m = "{}"
			     | otherwise = "{\n" ++ showKvs (n + 1) (Map.assocs m) ++ indent n ++ "}"

showArryElem :: Int -> [JSON] -> String
showArryElem _ [] = []
showArryElem n [a] = indent n ++ showWithIndent n a ++ "\n"
showArryElem n (a:as) = indent n ++ showWithIndent n a ++ ",\n" ++ showArryElem n as

showKvs :: Int ->  [(String, JSON)] -> String 
showKvs n [] = []
showKvs n [(s, j)] = indent n ++ showWithIndent n (JString s) ++ ": " ++ showWithIndent n j ++ "\n"
showKvs n ((s, j): kvs) = indent n ++ showWithIndent n (JString s) ++ ": " ++ showWithIndent n j ++ ",\n" ++ showKvs n kvs

indent :: Int -> String
indent n = take (2 * n) (repeat ' ')

instance Eq JSON where 
  JNull == JNull = True 
  JBool b == JBool a = a == b
  JNum a == JNum b = a == b
  JString a == JString b = a == b
  JArray a == JArray b = a == b
  JObject a == JObject b = a == b
  _ == _ = False



instance Ord JSON where
  compare JNull JNull = EQ
  compare JNull _ = LT
  compare _ JNull = GT
  compare (JBool a) (JBool b) = compare a b
  compare (JBool _) _ = LT
  compare _ (JBool _) = GT
  compare (JNum a) (JNum b) = compare a b
  compare (JNum _) _ = LT
  compare _ (JNum _) = GT
  compare (JString a) (JString b) = compare a b 
  compare (JString _) _ = LT
  compare _ (JString _) = GT
  compare (JArray a) (JArray b) = compare a b 
  compare (JArray _) _ = LT
  compare _ (JArray _) = GT
  compare (JObject a) (JObject b) = compare a b 

  
  
