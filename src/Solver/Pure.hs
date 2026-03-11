module Solver.Pure (solve) where

import qualified Data.Map.Strict as M
import Data.List (find)
import Types (Literal(..), Formula, Assignment, Variable)
import Control.Applicative (Alternative((<|>)))

findUnitClause :: Formula -> Maybe Literal
findUnitClause formula = case find (\c -> length c == 1) formula of
  Just [literal] -> Just literal
  _ -> Nothing

setLiteral :: Literal -> Assignment -> Assignment
setLiteral (Pos v) = M.insert v True
setLiteral (Neg v) = M.insert v False

negateLiteral :: Literal -> Literal
negateLiteral (Pos v) = Neg v
negateLiteral (Neg v) = Pos v

simplify :: Literal -> Formula -> Formula
simplify literal = map (filter (/= negateLiteral literal)) . filter (notElem literal)

chooseVariable :: Formula -> Variable
chooseVariable ((Pos variable:_):_) = variable
chooseVariable ((Neg variable:_):_) = variable
chooseVariable _ = error "chooseVariable: no variables left"

tryAssignment :: Formula -> Assignment -> Literal -> Maybe Assignment
tryAssignment formula assignment literal =
    let assignment'  = setLiteral literal assignment
        formula' = simplify literal formula
    in  dpll formula' assignment'

dpll :: Formula -> Assignment -> Maybe Assignment
dpll formula assignment
    | null formula = Just assignment
    | any null formula = Nothing
    | otherwise = case findUnitClause formula of
        Just literal -> tryAssignment formula assignment literal
        Nothing -> let variable = chooseVariable formula in
            tryAssignment formula assignment (Pos variable) <|>
            tryAssignment formula assignment (Neg variable)

solve :: Formula -> Maybe Assignment
solve f = dpll f M.empty
