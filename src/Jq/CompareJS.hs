module Jq.CompareJS where

import Jq.Json

data OrderingJS = COM Ordering 
                | NC

compareJs :: [JSON] -> [JSON]-> OrderingJS
compareJs [JNum n] [JNum m] = COM c
    where 
        c = compare n m  
compareJs [JBool b] [JBool c] = COM (compare b c)
compareJs [JString s] [JString t] = COM (compare s t)
compareJs _ _ = NC

instance Show OrderingJS where
    show (COM o) = "com " ++ show o
    show NC = "nc"



