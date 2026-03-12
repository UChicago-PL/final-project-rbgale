module Main where

import System.Environment (getArgs)
import System.FilePath (takeExtension)
import Types (Problem(..), Solve, Assignment, Formula)
import Parser.DIMACS (parseDIMACS)
import Parser.Sudoku (parseSudoku)
import Encoder.Sudoku (encodeSudoku)
import Solver.Pure as Pure (solve)
import Solver.Mutable as Mutable (solve)
import Printing (printAssignment, printGrid)
import Verify (verify)


data Options = Options {solver :: Solve, shouldVerify :: Bool}

defaultOptions :: Options
defaultOptions = Options {solver = Mutable.solve, shouldVerify = False}

parseOptions :: [String] -> Maybe (Options, String)
parseOptions = go defaultOptions where
    go _ [] = Nothing
    go options [filename] = Just (options, filename)
    go options ("--pure":rest) = go (options {solver = \_ f -> Pure.solve f}) rest
    go options ("--mutable":rest) = go (options {solver = Mutable.solve}) rest
    go options ("--verify":rest) = go (options {shouldVerify = True}) rest
    go _ _ = Nothing

solveCNF :: Options -> String -> String -> IO ()
solveCNF options filename input = case parseDIMACS filename input of
    Left err -> putStrLn err
    Right problem -> case solver options (numVars problem) (formula problem) of
        Nothing -> putStrLn "Unsatisfiable!"
        Just assignment -> do
            putStrLn $ printAssignment assignment
            verifyIf options assignment (formula problem)

solveSudoku :: Options -> String -> String -> IO ()
solveSudoku options filename input = case parseSudoku filename input of
    Left err -> putStrLn err
    Right grid -> let (problem, decoder) = encodeSudoku grid in
        case solver options (numVars problem) (formula problem) of
            Nothing -> putStrLn "No solution."
            Just assignment -> do
                putStrLn $ printGrid $ decoder assignment
                verifyIf options assignment (formula problem)

verifyIf :: Options -> Assignment -> Formula -> IO ()
verifyIf options assignment formula = if shouldVerify options then
    putStrLn $ if Verify.verify assignment formula then "Solution is correct."
        else "Solution is invalid (that wasn't supposed to happen)!"
    else pure ()

main :: IO ()
main = do
    args <- getArgs
    case parseOptions args of
        Just (options, filename) -> do
            input <- readFile filename
            case takeExtension filename of
                ".cnf" -> solveCNF options filename input
                ".sdk" -> solveSudoku options filename input
                ext -> putStrLn $ "Unknown file extension: " ++ ext
        Nothing -> putStrLn "Usage: sat-solver [--pure | --mutable] [--verify] <filename>"
