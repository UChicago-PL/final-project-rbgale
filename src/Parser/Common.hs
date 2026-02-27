module Parser.Common (Parser, parsePretty) where

import Text.Megaparsec (Parsec, parse, errorBundlePretty)
import Data.Void (Void)

type Parser = Parsec Void String

stripComments :: Char -> String -> String
stripComments marker = unlines . filter notComment . lines where
    notComment (c:_) = c /= marker
    notComment _ = False

parsePretty :: Char -> Parser a -> String -> String -> Either String a
parsePretty marker p filename input =
    case parse p filename (stripComments marker input) of
        Left err -> Left (errorBundlePretty err)
        Right val -> Right val
