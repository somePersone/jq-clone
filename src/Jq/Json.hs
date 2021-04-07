module Jq.Json where

import qualified Data.Map as Map

data JSON = JNull
          | JBool Bool  
          | JNum Float 
          | JString String 
          | JArray [JSON]
          | JObject (Map.Map String JSON)
      
instance Show JSON where
  show JNull = "null" 
  show (JBool True ) = "true"
  show (JBool False) = "false"
  show (JNum n) = if fromInteger i == n then show i else show n
    where
      i = round n :: Integer
  show (JString s) = "\"" ++ s ++ "\""
  show (JArray []) = "[]"   
  show (JArray a) = show a
  show (JObject m) = "{" ++ showKvs (Map.assocs m) ++ "}"

showKvs :: [(String, JSON)] -> String 
showKvs [] = []
showKvs [(s, j)] = show (JString s) ++ ": " ++ show j ++ ""
showKvs ((s, j): kvs) = show (JString s) ++ ": " ++ show j ++ ", " ++ showKvs kvs

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

  
  
