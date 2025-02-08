module REPL where

import Expr
import Parsing

data REPLState = REPLState { vars :: [(Name, Int)],
                             history :: [Command] }

initREPLState :: REPLState
initREPLState = REPLState [] []

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Int -> [(Name, Int)] -> [(Name, Int)]
updateVars name newValue vars = (name, newValue) : filter (\(x, _) -> x /= name) vars 
-- filter is a Prelude function that takes a function and a list
-- and returns the elements of the list that satisfy that condition

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Int)] -> [(Name, Int)]
dropVar name vars = filter (\(x,_) -> x /= name) vars

-- Add a command to the command history in the state
addHistory :: REPLState -> Command -> REPLState
addHistory st cmd = st {history = history st ++ [cmd]} 

process :: REPLState -> Command -> IO ()
process st (Set var e) 
     = do let st' = undefined
          -- st' should include the variable set to the result of evaluating e
          repl st'
-- repl print result when evluating expression         
process st (Eval e) = 
     case eval (vars st) e of
         Just v -> do
             print v
             repl st
         Nothing -> do
             putStrLn "Evaluation error!"
             repl st


-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: REPLState -> IO ()
repl st = do putStr (show (length (history st)) ++ " > ")
             inp <- getLine
             case parse pCommand inp of
                  [(Quit, "")] -> putStrLn "Bye"  -- Exit if user types `:q`
                  [(cmd, "")] -> -- Must parse entire input
                          process st cmd
                  _ -> do putStrLn "Parse error"
                          repl st

