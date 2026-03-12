module Parser.Sudoku (parseSudoku) where

import Text.Megaparsec (optional, oneOf, count, (<|>), MonadParsec(eof))
import Text.Megaparsec.Char (char, newline)
import Data.Char (digitToInt)
import qualified Data.Map.Strict as M
import Parser.Common (Parser, parsePretty)
import Types (SudokuGrid)

cell :: Parser (Maybe Int)
cell = (Just . digitToInt <$> oneOf ['1'..'9']) <|> (Nothing <$ char '.')

row :: Parser [Maybe Int]
row = count 9 cell <* optional newline

grid :: Parser [[Maybe Int]]
grid = count 9 row <* eof

toSudokuGrid :: [[Maybe Int]] -> SudokuGrid
toSudokuGrid rows = M.fromList [
    ((r, c), d) |
    (r, rowCells) <- zip [0..] rows,
    (c, Just d) <- zip [0..] rowCells]

parseSudoku :: String -> String -> Either String SudokuGrid
parseSudoku filename input = toSudokuGrid <$> parsePretty "#" grid filename input
