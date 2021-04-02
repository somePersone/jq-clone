{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Jq.Opperations where

import Jq.Filters 
import Jq.Json  

opperate :: Opp -> JSON -> JSON -> JSON
opperate Add = addition
opperate Sub = subtraction
opperate Div = division
opperate Mult = multiplication

addition :: JSON -> JSON -> JSON
addition (JNum n) (JNum m) = JNum (n + m)
addition (JArray ls) (JArray rs) = JArray (ls ++ rs) 
addition (JString l) (JString r) = JString (l ++ r)
addition (JObject lkvs) (JObject rkvs) = JObject ([lkv | lkv <- lkvs, case lookup (fst lkv) rkvs of 
                                                                                Nothing -> True 
                                                                                _ -> False] ++ rkvs)
addition JNull r = r
addition l JNull = l



subtraction :: JSON -> JSON -> JSON 
subtraction (JNum n) (JNum m) = JNum (n - m)
-- subtraction (JArray ls) (JArray rs) = JArray [l | l <- ls, l `elem` rs]


multiplication :: JSON -> JSON -> JSON 
multiplication (JNum n) (JNum m) = JNum (n * m)
multiplication (JString l) (JNum n) | n <= 0 = JNull
                                    | otherwise = JString (multString l n)
    where
        multString :: String -> Float -> String
        multString s n | n <= 0 = []
                       | otherwise = s ++ multString s (n - 1)
-- /

    -- JObject ([kv | lkv <- lkvs, case lookup (fst lkv) rkvs of 
    --                                                                             Nothing -> Just kv 
    --                                                                             Just (JObject nkvs) -> Just multiplication nkvs lkv
    --                                                                             _ -> Nothing] ++ rkvs)                     


division :: JSON -> JSON -> JSON 
division (JNum n) (JNum m) | m /= 0.0  = JNum (n / m)
                           | otherwise = error "devision by 0"
-- division (JString ls) (JString rs) = JArray (DivString ls rs)
--     where
--         divString :: [Char] -> [Char] -> [String]  
--         divString [] _ = []
--         divString (l:ls) rs | rs `isPrefixOf` (l:ls) = divString ((l:ls) \\ rs) rs
--                         | otherwise = l 