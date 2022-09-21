module Lib
    ( readExpr, symbol
    ) where

import Text.Parsec hiding (spaces)
import Text.Parsec.String (Parser)

readExpr :: String -> String
readExpr input = 
    case parse (spaces >> symbol) "lisp" input of
        Left err -> "No match: " ++ show err
        Right _ -> "Found value"

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space



