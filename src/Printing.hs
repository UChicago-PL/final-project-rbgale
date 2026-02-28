module Printing (printAssignment, printGrid) where

import qualified Data.Map.Strict as M
import Types (Assignment, SudokuGrid)

printAssignment :: Assignment -> String
printAssignment assignment
    | M.null assignment = "Satisfiable (no variables)"
    | otherwise = "Satisfiable\n" ++ unlines
        ["  x" ++ show var ++ " = " ++ show val | (var, val) <- M.toAscList assignment]

printGrid :: SudokuGrid -> String
printGrid grid = unlines (interleaveRows (map formatRow [0..8])) where
    cellStr r c = case M.lookup (r, c) grid of
        Just d -> show d
        Nothing -> "."

    formatRow r =
        let cells = [cellStr r c | c <- [0..8]]
            (b1, b2, b3) = split3 cells
        in unwords b1 ++ " | " ++ unwords b2 ++ " | " ++ unwords b3

    interleaveRows rows =
        let (top, mid, bot) = split3 rows
            separator = "------+-------+------"
        in top ++ [separator] ++ mid ++ [separator] ++ bot

    split3 xs =
        let (a, rest) = splitAt 3 xs
            (b, c) = splitAt 3 rest
        in (a, b, c)
