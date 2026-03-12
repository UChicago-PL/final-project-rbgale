module Parser.Common (Parser, parsePretty) where

import Text.Megaparsec (Parsec, parse, errorBundlePretty)
import Data.Void (Void)

type Parser = Parsec Void String

stripCommentsAndBlanks :: String -> String -> String
stripCommentsAndBlanks marker = unlines . filter notComment . lines where
    notComment (c:_) = c `notElem` marker
    notComment _ = False

parsePretty :: String -> Parser a -> String -> String -> Either String a
parsePretty marker p filename input =
    case parse p filename (stripCommentsAndBlanks marker input) of
        Left err -> Left (errorBundlePretty err)
        Right val -> Right val
