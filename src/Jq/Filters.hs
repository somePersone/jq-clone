module Jq.Filters where

import Jq.Json  

data Filter = Identity
            | ArrayIndex Int 
            | ArraySlice (Maybe Int) (Maybe Int)
            | ItterateAll
            | ItterateSelect [Int]
            | Parenthesis Filter
            | ObjectIndex String
            | Optional Filter
            | Comma Filter Filter
            | Pipe Filter Filter
            | Array Filter
            | Object [ObjectItem]
            | Value JSON
            | IfThenElse Filter Filter Filter
            | RecursiveDecent
            | Opperation Opp Filter Filter
	    | Comparisons Comp Filter Filter

data Opp = Add
         | Sub
         | Mul
         | Div

data Comp = Equal
	  | NotEqual
	  | LessThan
	  | LessThanEqual
	  | GreaterThan
	  | GreaterThanEqual


data ObjectItem = StringValue String Filter
                | ValueValue Filter Filter         

instance Show Filter where
  show Identity = "."
  show (ArrayIndex n) = ".[" ++ show n ++ "]" 
  show (ArraySlice n m) = ".[" ++ showMaybe n  ++ ":" ++ showMaybe m ++ "]"
    where 
      showMaybe Nothing = ""
      showMaybe (Just x) = show x
  show ItterateAll = ".[]"
  show (ItterateSelect ns) = "." ++ show ns
  show (Parenthesis f) = "(" ++ show f ++ ")"
  show (ObjectIndex s) = "." ++ show s 
  show (Optional f) = show f ++ "?"
  show (Comma l r) = show l ++ "," ++ show r
  show (Pipe l r) = show l ++ "|" ++ show r
  show (Array f) = "[" ++ show f ++ "]" 
  show (Object os) = "{" ++ showOs os ++ "}"
    where 
      showOs [] = "" 
      showOs [sv] = show sv
      showOs (sv : svs) = show sv ++ "," ++ showOs svs
  show (Value v) = show v
  show (IfThenElse i t e) = "If " ++ show i ++ " then " ++ show t ++ " else " ++ show e 
  show RecursiveDecent = ".."
  show (Opperation o l r) = show l ++ show o ++ show r
  show (Comparisons c l r) = show l ++ show c ++ show r


instance Show Opp where
  show Add = " + "
  show Sub = " - "
  show Mul = " * "
  show Div = " / "

instance Show Comp where
  show Equal = " == "
  show NotEqual = " != "
  show LessThan = " < "
  show LessThanEqual = " <= "
  show GreaterThan = " > "
  show GreaterThanEqual = " >= "
  
instance Show ObjectItem where
  show (StringValue s f) = show s ++ ":" ++ show f
  show (ValueValue l r) = show l ++ ":" ++ show r

data Config = ConfigC {filters :: Filter}
