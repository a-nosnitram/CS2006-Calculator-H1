module Main where

import Parsing
import Expr
import REPL
import System.Environment (getArgs)

main :: IO ()
main = do 
       args <- getArgs 
       case args of 
            [] -> repl initREPLState
            [fileName] -> runFile fileName

-- This function reads a file, retieves its content, splitting it into lines, 
-- and then calls runCommands on the contents of the file 
runFile :: String -> IO ()
runFile fileName = do 
                   contents <- readFile fileName -- retrieving file contetnts 
                   let commands = lines contents -- splitting file into lines
                   putStrLn ("reading from file " ++ fileName ++ "\n")
                   runCommands commands initREPLState 1 -- 0 is the number of the line we're on

-- This function similar to repl in REPL.hs, but just runs untill the end of the file insltead 
-- of looping back and quits automatically when it has executed all commands 
-- it also prints out the command before printing out its result 
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
                  
