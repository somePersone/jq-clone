{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Jq.Opperations where

import Jq.Filters 
import Jq.Json  
import qualified Data.Map as Map


opperate :: Opp -> JSON -> JSON -> JSON
opperate Add = addition
opperate Sub = subtraction
opperate Div = division
opperate Mul = multiplication

addition :: JSON -> JSON -> JSON
addition (JNum n) (JNum m) = JNum (n + m)
addition (JArray ls) (JArray rs) = JArray (ls ++ rs) 
addition (JString l) (JString r) = JString (l ++ r)
addition (JObject l) (JObject r) = JObject (Map.union r l)
addition JNull r = r
addition l JNull = l



subtraction :: JSON -> JSON -> JSON 
subtraction (JNum n) (JNum m) = JNum (n - m)
subtraction (JArray ls) (JArray rs) = JArray [l | l <- ls, not (l `elem` rs)]


multiplication :: JSON -> JSON -> JSON 
multiplication (JNum n) (JNum m) = JNum (n * m)
multiplication (JString l) (JNum n) | n <= 0 = JNull
                                    | otherwise = JString (multString l n)
    where
        multString :: String -> Float -> String
        multString s n | n <= 0 = []
                       | otherwise = s ++ multString s (n - 1)
multiplication (JObject l) (JObject r) = JObject (Map.unionWith (multiplication) r l) 	

division :: JSON -> JSON -> JSON 
division (JNum n) (JNum m) | m /= 0.0  = JNum (n / m)
                           | otherwise = error "devision by 0"
-- division (JString ls) (JString rs) = JArray (divString ls rs)
--     where
-- 	divString :: [Char] -> [Char] -> [String]  
-- 	divString [] _ = []
-- 	divString ls rs | not (rs `isInfixOf` ls) = [[ls]]
-- 			| rs `isPrefixOf` ls = ls \\ rs
-- 			| otherwise = [[ls]]
