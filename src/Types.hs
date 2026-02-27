module Types (
    Variable,
    Literal(..),
    Clause,
    Formula,
    Assignment,
    Problem(..),
    SudokuGrid
) where

import qualified Data.Map.Strict as M

type Variable = Int
data Literal = Pos Variable | Neg Variable
    deriving (Show, Eq, Ord)
type Clause = [Literal]
type Formula = [Clause]
type Assignment = M.Map Variable Bool

data Problem = Problem {
    numVars :: Int,
    numClauses :: Int,
    formula :: Formula
} deriving (Show)

-- Keys are (row, col) pairs with 0-indexing; entries are 1-9 and empty squares
-- are not stored
type SudokuGrid = M.Map (Int, Int) Int
