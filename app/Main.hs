module Main where

import System.Environment (getArgs)
import System.FilePath (takeExtension)
import Types (Problem(..), Solve)
import Parser.DIMACS (parseDIMACS)
import Parser.Sudoku (parseSudoku)
import Encoder.Sudoku (encodeSudoku)
import Solver.Pure as Pure (solve)
import Solver.Mutable as Mutable (solve)
import Printing (printAssignment, printGrid)

solveCNF :: Solve -> String -> String -> IO ()
solveCNF solver filename input = case parseDIMACS filename input of
    Left err -> putStrLn err
    Right problem -> case solver (numVars problem) (formula problem) of
        Nothing -> putStrLn "Unsatisfiable!"
        Just assignment -> putStrLn $ printAssignment assignment

solveSudoku :: Solve -> String -> String -> IO ()
solveSudoku solver filename input = case parseSudoku filename input of
    Left err -> putStrLn err
    Right grid -> let (problem, decoder) = encodeSudoku grid in
        case solver (numVars problem) (formula problem) of
            Nothing -> putStrLn "No solution."
            Just assignment -> putStrLn $ printGrid $ decoder assignment

parseSolver :: String -> Maybe Solve
parseSolver "--pure" = Just (\_ f -> Pure.solve f)
parseSolver "--mutable" = Just Mutable.solve
parseSolver _ = Nothing

main :: IO ()
main = do
    args <- getArgs
    case args of
        [flag, filename] | Just solver <- parseSolver flag -> do
            input <- readFile filename
            case takeExtension filename of
                ".cnf" -> solveCNF solver filename input
                ".sdk" -> solveSudoku solver filename input
                ext -> putStrLn $ "Unknown file extension: " ++ ext
        [filename] -> do
            input <- readFile filename
            case takeExtension filename of
                ".cnf" -> solveCNF Mutable.solve filename input
                ".sdk" -> solveSudoku Mutable.solve filename input
                ext -> putStrLn $ "Unknown file extension: " ++ ext
        _ -> putStrLn "Usage: sat-solver [--pure | --mutable] <filename>"
