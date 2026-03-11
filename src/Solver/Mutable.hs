module Solver.Mutable (solve) where

import Types (Assignment, Formula, Variable, Literal (..), Clause)
import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, newArray, readArray, writeArray)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)
import qualified Data.Map.Strict as M
import Control.Monad (forM_)

data SolverState s = SolverState {
    assignments :: STArray s Variable (Maybe Bool),
    trail :: STRef s [Variable]
}

data ScanResult = Conflict | UnitFound Literal | NothingFound

newSolverState :: Int -> ST s (SolverState s)
newSolverState n = do
    assignments <- newArray (1, n) Nothing
    trail <- newSTRef []
    pure SolverState {assignments = assignments, trail = trail}

literalValue :: SolverState s -> Literal -> ST s (Maybe Bool)
literalValue st (Pos var) = readArray (assignments st) var
literalValue st (Neg var) = fmap not <$> readArray (assignments st) var

evalClause :: SolverState s -> Clause -> ST s (Maybe Bool, Maybe Literal)
evalClause st = go Nothing where
    go unassigned [] = case unassigned of
        Just lit -> pure (Nothing, Just lit)
        Nothing -> pure (Just False, Nothing)
    go unassigned (lit:lits) = do
        val <- literalValue st lit
        case val of
            Just True -> pure (Just True, Nothing)
            Just False -> go unassigned lits
            Nothing -> case unassigned of
                Nothing -> go (Just lit) lits
                Just _ -> skipRest lits

    skipRest [] = pure (Nothing, Nothing)
    skipRest (lit:lits) = do
        val <- literalValue st lit
        case val of
            Just True -> pure (Just True, Nothing)
            _ -> skipRest lits

scanFormula :: SolverState s -> Formula -> ST s ScanResult
scanFormula st = go where
    go [] = pure NothingFound
    go (clause:clauses) = do
        (status, mLit) <- evalClause st clause
        case status of
            Just False -> pure Conflict
            Just True -> go clauses
            Nothing -> case mLit of
                Just lit -> pure (UnitFound lit)
                Nothing -> go clauses

chooseVariable :: SolverState s -> Int -> ST s (Maybe Variable)
chooseVariable st n = go 1 where
    go var | var > n = pure Nothing
    go var = do
        val <- readArray (assignments st) var
        case val of
            Nothing -> pure (Just var)
            Just _  -> go (var + 1)

modifyTrail :: SolverState s -> Variable -> ST s ()
modifyTrail st var = readSTRef (trail st) >>= writeSTRef (trail st) . (var :)

assignLiteral :: SolverState s -> Literal -> ST s ()
assignLiteral st (Pos var) = do
    writeArray (assignments st) var (Just True)
    modifyTrail st var
assignLiteral st (Neg var) = do
    writeArray (assignments st) var (Just False)
    modifyTrail st var

saveTrail :: SolverState s -> ST s [Variable]
saveTrail st = readSTRef (trail st)

restoreTrail :: SolverState s -> [Variable] -> ST s ()
restoreTrail st saved = do
    current <- readSTRef (trail st)
    let numToUndo = length current - length saved
        (toUndo, _) = splitAt numToUndo current
    forM_ toUndo $ \var -> writeArray (assignments st) var Nothing
    writeSTRef (trail st) saved

dpll :: SolverState s -> Formula -> Int -> ST s Bool
dpll st formula n = do
    result <- scanFormula st formula
    case result of
        Conflict -> pure False
        UnitFound lit -> do
            assignLiteral st lit
            dpll st formula n
        NothingFound -> do
            mVar <- chooseVariable st n
            case mVar of
                Nothing -> pure True
                Just var -> do
                    saved <- saveTrail st
                    assignLiteral st (Pos var)
                    noConflict <- dpll st formula n
                    if noConflict then pure True
                    else do
                        restoreTrail st saved
                        assignLiteral st (Neg var)
                        dpll st formula n

extractAssignment :: SolverState s -> Int -> ST s Assignment
extractAssignment st n = M.fromList <$> collect 1 where
    collect var | var > n = pure []
    collect var = do
        val <- readArray (assignments st) var
        case val of
            Just b -> ((var, b) :) <$> collect (var + 1)
            Nothing -> collect (var + 1)

solve :: Int -> Formula -> Maybe Assignment
solve n formula = runST $ do
    st <- newSolverState n
    result <- dpll st formula n
    if result then Just <$> extractAssignment st n
    else pure Nothing
