module Jq.Compiler where

import           Jq.Filters
import           Jq.Json
import           Jq.Opperations
import qualified Data.Map as Map



type JProgram a = JSON -> Either String a

compile :: Filter -> JProgram [JSON]

compile Identity inp = return [inp]

compile (Parenthesis f) inp = compile f inp

compile (ArrayIndex _) JNull = return [JNull]
compile (ArrayIndex n) (JArray a) =  return [a !! getIndex n a] 

compile (ObjectIndex _) JNull = return [JNull]
compile (ObjectIndex k) (JObject kv) =  return (case kv Map.!? k of
						 Nothing -> [JNull]
						 Just a -> [a])


compile (ArraySlice _ _) JNull = return [JNull]
compile (ArraySlice n m) (JArray a) = return [JArray[a !! x | x <- getIndexs n m a, x < length a]]
compile (ArraySlice n m) (JString a) = return [JString [a !! x | x <- getIndexs n m a, x < length a]]

compile ItterateAll (JArray as) = return as
compile ItterateAll (JObject m) = return (Map.elems m)

compile (ItterateSelect _) JNull = return [JNull]
compile (ItterateSelect ns) (JArray as) = return [as !! getIndex x as | x <- ns, x < length as]
compile (ItterateSelect ns) (JObject m) = return (do
	n <- ns
	return (snd (Map.elemAt n m)))

compile (Optional f) inp = case compile f inp of 
                            Left _ -> return []
                            Right j -> return j

compile (Comma l r) inp = (++) <$> compile l inp <*> compile r inp   

compile (Pipe l r) inp =  concat <$> (compile l inp  >>= traverse (compile r))

compile (Value json) _ = return [json]

compile (Array f) inp = (:[]) . JArray <$> compile f inp

compile RecursiveDecent (JArray js) = (JArray js :) <$>  (concat <$> traverse (compile RecursiveDecent) js)
compile RecursiveDecent (JObject m) = (JObject m :) <$>  (concat <$> traverse (compile RecursiveDecent) (Map.elems m))
compile RecursiveDecent a = return [a]

compile (Opperation o l r) inp = case (compile l inp, compile r inp) of
                                    (Right [left], Right [right]) -> return [opperate o left right]
                                    (Left s, Left s1) -> Left (s ++ s1)
                                    (Left s, _) -> Left s
                                    (_, Left s) -> Left s
                                    (_, _) -> Left "More then 1 opp"

compile (Comparisons c l r) inp = case (compile l inp, compile r inp) of
                                    (Right [left], Right [right]) -> return [JBool (case c of
						Equal -> left == right
						NotEqual -> left /= right
						LessThan -> left < right
						LessThanEqual -> left <= right
						GreaterThan -> left > right
						GreaterThanEqual -> left >= right
						And -> (truthy left) && (truthy right)
						Or -> (truthy left) || (truthy right))]
                                    (Left s, Left s1) -> Left (s ++ s1)
                                    (Left s, _) -> Left s
                                    (_, Left s) -> Left s
                                    (_, _) -> Left "More then 1 value"

compile (Not) inp = return [case truthy inp of
			True -> JBool False
			False -> JBool True]

compile (Object as) inp = 
    do 
    oiss <- traverse com as
    let xss = foldr combineLists [[]] oiss :: [[(String, JSON)]]
    return (JObject . Map.fromList <$> xss)
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
                                    Right [j] -> if truthy j then compile t inp else compile e inp
				    Right _ -> Left "More then one predicate"
                                    Left s -> Left s        

compile _ _ = Left "Error"
    
getIndexs :: Maybe Int -> Maybe Int -> [a] -> [Int]
getIndexs Nothing Nothing as = [0..(length as)]
getIndexs (Just n) Nothing as = [(getIndex n as)..(length as - 1)]
getIndexs Nothing (Just m) as = [0..(getIndex m as - 1)]
getIndexs (Just n) (Just m) as = [(getIndex n as)..(getIndex m as - 1)]

getIndex :: Int -> [a] -> Int
getIndex n a = if n >= 0 then n else length a + n

truthy :: JSON -> Bool
truthy JNull = False
truthy (JBool False) = False
truthy _ = True			

run :: JProgram [JSON] -> JSON -> Either String [JSON]
run p j = p j
