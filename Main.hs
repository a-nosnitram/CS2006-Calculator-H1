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

runFile :: String -> IO ()
runFile fileName = do 
                   contents <- readFile fileName -- retrieving file contetnts 
		   let commands = lines contents -- splitting file into lines
		   putStrLn ("reading from file " ++ fileName)
		   runCommands commands initREPLState

-- similar to repl in REPL.hs, but just runs untill the end of the file insltead 
-- of looping back and quits automatically when it has executed all commands 
-- it also prints out the command before printing out its result 
runCommands :: [String] -> REPLState -> IO ()
runCommands [] st = process st Quit 
runCommands (cmd:cmds) st = do 
        if not (all (\c -> c `elem` " \t") cmd) 
             then putStr $ show (length (history st)) ++ " > " ++ cmd ++ "\n"
             else return ()

        case parse pCommand cmd of 
             [(command, "")] -> do 
		st' <- processFileLine st command
		runCommands cmds st'
             _ -> do 
		putStrLn "file parse error"
                runCommands cmds st
                  
