module Jq.KParser where

import Parsing.Parsing

parseKey :: Parser String
parseKey = do
    c <- letter <|> char '_'
    cs <- many (alphanum <|> char '_')
    return (c : cs)