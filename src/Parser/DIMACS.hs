module Parser.DIMACS where

import Text.Megaparsec
import Types (Problem)

-- file name only needed for error messages
parseDIMACS :: String -> String -> Either String Problem
parseDIMACS = undefined
