module Main where

import Parsing
import Expr
import REPL
import System.Environment (getArgs)

-- Entry point of the program
main :: IO ()
main = do 
       args <- getArgs 
       case args of 
            [] -> repl initREPLState
            [fileName] -> runFile fileName

-- Function to read and execute commands from a file 
runFile :: String -> IO ()
runFile fileName = do 
                   contents <- readFile fileName -- retrieving file contetnts 
                   let commands = lines contents -- splitting file into lines
                   putStrLn ("reading from file " ++ fileName ++ "\n")
                   runCommands commands initREPLState 1 -- the number of first line

-- Function to execute a list of commands from a file
-- Similar to 'repl' in REPL.hs but processes all commands sequentially
-- Automatically exits when all commands are executed
runCommands :: [String] -> REPLState -> Int -> IO ()
runCommands [] st l = putStrLn "\n file reading complete"
runCommands (cmd:cmds) st l = do 
        case parse pCommand cmd of 
             [(command, "")] -> do 
                st' <- processFileLine st command (l+1) 
                runCommands cmds st' (l+1)
             _ -> do 
                putStrLn ("File parse error at line " ++ show l)
                runCommands cmds st (l+1)
                  
