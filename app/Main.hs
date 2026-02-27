module Main where

import System.Environment (getArgs)
import System.FilePath (takeExtension)

import Types (Problem(..))
import Parser.DIMACS (parseDIMACS)
import Parser.Sudoku (parseSudoku)
import Encoder.Sudoku (encodeSudoku)
import Solver (solve)
import Printing (printAssignment, printGrid)

solveCNF :: String -> String -> IO ()
solveCNF filename input = case parseDIMACS filename input of
    Left err -> putStrLn err
    Right problem -> case solve (formula problem) of
        Nothing -> putStrLn "Unsatisfiable!"
        Just assignment -> putStrLn $ printAssignment assignment

solveSudoku :: String -> String -> IO ()
solveSudoku filename input = case parseSudoku filename input of
    Left err -> putStrLn err
    Right grid -> let (problem, decoder) = encodeSudoku grid in
        case solve (formula problem) of
            Nothing -> putStrLn "No solution."
            Just assignment -> putStrLn $ printGrid $ decoder assignment

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> do
            input <- readFile filename
            case takeExtension filename of
                ".cnf" -> solveCNF filename input
                ".sdk" -> solveSudoku filename input
                ext -> putStrLn $ "Uknown file extension: " ++ ext
        _ -> putStrLn "Usage: sat-solver <filename>"
