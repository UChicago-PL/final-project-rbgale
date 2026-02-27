module Parser.DIMACS (parseDIMACS) where

import Text.Megaparsec (many, some, MonadParsec(eof, try))
import Text.Megaparsec.Char (string, char, space)
import qualified Text.Megaparsec.Char.Lexer as L
import Parser.Common (Parser, parsePretty)
import Types (Problem(..), Literal(..), Clause)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme space

problemLine :: Parser (Int, Int)
problemLine = do
  _ <- lexeme (char 'p')
  _ <- lexeme (string "cnf")
  numVars <- lexeme L.decimal
  numClauses <- lexeme L.decimal
  pure (numVars, numClauses)

literal :: Parser Int
literal = try $ do
  n <- lexeme (L.signed (pure ()) L.decimal)
  if n == 0
    then fail "expected nonzero literal"
    else pure n

toLiteral :: Int -> Literal
toLiteral n
  | n > 0 = Pos n
  | n < 0 = Neg (abs n)
  | otherwise = error "toLiteral: unexpected zero"

clause :: Parser Clause
clause = do
  literals <- some literal
  _ <- lexeme (char '0')
  pure (map toLiteral literals)

dimacs :: Parser Problem
dimacs = do
  space
  (numVars, numClauses) <- problemLine
  clauses <- many clause
  eof
  pure $ Problem {numVars = numVars, numClauses = numClauses, formula = clauses}

-- file name only needed for error messages
parseDIMACS :: String -> String -> Either String Problem
parseDIMACS = parsePretty 'c' dimacs
