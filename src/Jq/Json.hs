module Jq.Json where

data JSON = JNull
          | JBool Bool  
          | JNum Float 
          | JString String 
          | JArray [JSON]
          | JObject [(String, JSON)]
      
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
  show (JObject kvs) = "{\n " ++ showKvs kvs ++ "}"

showKvs :: [(String, JSON)] -> String 
showKvs [] = []
showKvs [(s, j)] = show (JString s) ++ ": " ++ show j ++ "\n"
showKvs ((s, j): kvs) = show (JString s) ++ ": " ++ show j ++ ",\n " ++ showKvs kvs