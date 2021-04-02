module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import           Jq.CompareJS
import           Jq.Opperations



type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]

compile Identity inp = return [inp]

compile (Parenthesis f) inp = compile f inp

compile (ArrayIndex _) JNull = return [JNull]
compile (ArrayIndex n) (JArray a) =  return [a !! getIndex n a] 

compile (ObjectIndex k) (JObject kv) =  return [case lookup k kv of 
    Nothing -> JNull
    Just j -> j]


compile (ArraySlice _ _) JNull = return [JNull]
compile (ArraySlice n m) (JArray a) = return [JArray[a !! x | x <- getIndexs n m a, x < length a]]
compile (ArraySlice n m) (JString a) = return [JString [a !! x | x <- getIndexs n m a, x < length a]]

compile ItterateAll (JArray as) = return as
compile ItterateAll (JObject kvs) = return (map snd kvs)

compile (ItterateSelect _) JNull = return [JNull]
compile (ItterateSelect ns) (JArray as) = return [as !! getIndex x as | x <- ns, x < length as]
compile (ItterateSelect ns) (JObject kvs) = return [snd (kvs !! getIndex x kvs) | x <- ns, x < length kvs]

compile (Optional f) inp = case compile f inp of 
                            Left _ -> return []
                            Right j -> return j

compile (Comma l r) inp = (++) <$> compile l inp <*> compile r inp   

compile (Pipe l r) inp =  concat <$> (compile l inp  >>= traverse (compile r))

compile (Value json) _ = return [json]

compile (Array f) inp = (:[]) . JArray <$> compile f inp

compile RecursiveDecent (JArray js) = (JArray js :) <$>  (concat <$> traverse (compile RecursiveDecent) js)
compile RecursiveDecent (JObject kvs) = (JObject kvs :) <$>  (concat <$> traverse (compile RecursiveDecent . snd) kvs)
compile RecursiveDecent a = return [a]

compile (Opperation o l r) inp = case (compile l inp, compile r inp) of
                                    (Right [left], Right [right]) -> return [opperate o left right]
                                    (Left s, Left s1) -> Left (s ++ s1)
                                    (Left s, _) -> Left s
                                    (_, Left s) -> Left s
                                    (_, _) -> Left "More then 1 opp"

-- compile (EqualToo l r) inp = case compileAndComper l r inp of
--   (Right (COM EQ)) -> Right [JBool True]
--   (Right _) -> Right [JBool False]
--   (Left s) -> Left s

-- compile (NotEqualToo l r) inp = case compileAndComper l r inp of
--   (Right (COM EQ)) -> Right [JBool False]
--   (Right _) -> Right [JBool True]
--   (Left s) -> Left s

-- compile (SmallerThan l r) inp = case compileAndComper l r inp of
--   Right (COM LT) -> Right [JBool True]
--   Right _ -> Right [JBool False]
--   Left s -> Left s

-- compile (SmallerEqualThan l r) inp = case compileAndComper l r inp of
--   Right (COM LT) -> Right [JBool True]
--   Right (COM EQ) -> Right [JBool True]
--   Right _ -> Right [JBool False]
--   Left s -> Left s

-- compile (LargerThan l r) inp = case compileAndComper l r inp of
--   Right (COM GT) -> Right [JBool True]
--   Right _ -> Right [JBool False]
--   Left s -> Left s

-- compile (LargerEqualThan l r) inp = case compileAndComper l r inp of
--   Right (COM GT) -> Right [JBool True]
--   Right (COM EQ) -> Right [JBool True]
--   Right _ -> Right [JBool False]
--   Left s -> Left s     



compile (Object as) inp = 
    do 
    oiss <- traverse com as
    let xss = foldr combineLists [[]] oiss :: [[(String, JSON)]]
    return (JObject <$> xss)
  where
      combineLists :: [a] -> [[a]] -> [[a]]
      combineLists xs bss = bss >>= \bs -> xs >>= \x -> [x:bs]
      com :: ObjectItem -> Either String [(String, JSON)]
      com (StringValue s f) = co s f
      com (ValueValue r l) = 
          case compile r inp of
            Left s -> Left s
            Right [JString n] -> co n l
            Right _ -> Left "not a name" 
      co :: String -> Filter -> Either String [(String, JSON)]
      co s f = do 
        cs <- compile f inp
        return (do 
            c <- cs
            return (s, c))


compile (IfThenElse i t e) inp = case compile i inp of
                                    Right [JBool True] -> compile t inp
                                    Right [JBool False] -> compile e inp
                                    Right _ -> Left "ivalid predicate for if"
                                    Left s -> Left s        

compile _ _ = Left "Error"
    

            
compileAndCompar :: Filter -> Filter -> JProgram OrderingJS
compileAndCompar l r inp = do
      left <- compile l inp
      right <- compile r inp
      return (compareJs left right)
    
getIndexs :: Maybe Int -> Maybe Int -> [a] -> [Int]
getIndexs Nothing Nothing as = [0..(length as)]
getIndexs (Just n) Nothing as = [(getIndex n as)..(length as - 1)]
getIndexs Nothing (Just m) as = [0..(getIndex m as - 1)]
getIndexs (Just n) (Just m) as = [(getIndex n as)..(getIndex m as - 1)]

getIndex :: Int -> [a] -> Int
getIndex n a = if n >= 0 then n else length a + n

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
