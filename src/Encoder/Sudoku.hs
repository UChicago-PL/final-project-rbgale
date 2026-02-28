module Encoder.Sudoku (encodeSudoku) where

import qualified Data.Map.Strict as M
import Types

-- unique variable number for each possible value at each position
toVar :: Int -> Int -> Int -> Variable
toVar r c d = r * 81 + c * 9 + (d - 1) + 1

atLeastOnePerCell :: Formula
atLeastOnePerCell = [[Pos (toVar r c d) | d <- [1..9]] | r <- [0..8], c <- [0..8]]

atMostOnePerCell :: Formula
atMostOnePerCell =
    [[Neg (toVar r c d1), Neg (toVar r c d2)] |
    r <- [0..8], c <- [0..8],
    d1 <- [1..9], d2 <- [d1+1..9]]

atLeastOnePerRow :: Formula
atLeastOnePerRow = [[Pos (toVar r c d) | c <- [0..8]] | r <- [0..8], d <- [1..9]]

atMostOnePerRow :: Formula
atMostOnePerRow =
    [[Neg (toVar r c1 d), Neg (toVar r c2 d)] |
    r <- [0..8], d <- [1..9],
    c1 <- [0..8], c2 <- [c1+1..8]]

atLeastOnePerCol :: Formula
atLeastOnePerCol = [[Pos (toVar r c d) | r <- [0..8]] | c <- [0..8], d <- [1..9]]

atMostOnePerCol :: Formula
atMostOnePerCol =
    [[Neg (toVar r1 c d), Neg (toVar r2 c d)] |
    c <- [0..8], d <- [1..9],
    r1 <- [0..8], r2 <- [r1+1..8]]

atLeastOnePerBox :: Formula
atLeastOnePerBox =
    [[Pos (toVar (br + dr) (bc + dc) d) | dr <- [0..2], dc <- [0..2]] |
    br <- [0, 3, 6], bc <- [0, 3, 6], d <- [1..9]]

atMostOnePerBox :: Formula
atMostOnePerBox =
    [[Neg (toVar (br + dr1) (bc + dc1) d), Neg (toVar (br + dr2) (bc + dc2) d)] |
    br <- [0, 3, 6], bc <- [0, 3, 6], d <- [1..9],
    (dr1, dc1) <- positions,
    (dr2, dc2) <- positions,
    (dr1, dc1) < (dr2, dc2)]
    where positions = [(dr, dc) | dr <- [0..2], dc <- [0..2]]

givenClauses :: SudokuGrid -> Formula
givenClauses grid = [[Pos (toVar r c d)] | ((r, c), d) <- M.toList grid]

fromVar :: Variable -> (Int, Int, Int)
fromVar v =
    let v' = v - 1
        r = v' `div` 81
        c = (v' `mod` 81) `div` 9
        d = (v' `mod` 9) + 1
    in (r, c, d)

encodeSudoku :: SudokuGrid -> (Problem, Assignment -> SudokuGrid)
encodeSudoku grid = (problem, decode) where
    allClauses = concat
        [atLeastOnePerCell,
        atMostOnePerCell,
        atLeastOnePerRow,
        atMostOnePerRow,
        atLeastOnePerCol,
        atMostOnePerCol,
        atLeastOnePerBox,
        atMostOnePerBox,
        givenClauses grid]

    problem = Problem {
        numVars = 9 * 9 * 9,
        numClauses = length allClauses,
        formula = allClauses
    }
    
    decode :: Assignment -> SudokuGrid
    decode assign = M.fromList
        [((r, c), d) |
        (var, True) <- M.toList assign,
        let (r, c, d) = fromVar var,
        r >= 0 && r <= 8,
        c >= 0 && c <= 8,
        d >= 1 && d <= 9]
