module REPL where

import Expr
import Expr (VarTree, lookupVar)
import Parsing
-- In REPL.hs
data REPLState = REPLState { 
  -- vars :: [(Name, Value)], 
  vars :: Expr.VarTree,
  history :: [Command], 
  lastResult :: Maybe Value  
}

initREPLState :: REPLState
-- initREPLState = REPLState [] [] Nothing
initREPLState = REPLState Empty [] Nothing

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
-- updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
-- updateVars name newValue vars = (name, newValue) : filter (\(x, _) -> x /= name) vars 
updateVars :: Name -> Value -> VarTree -> VarTree
updateVars name newValue Empty = Node name newValue Empty Empty
updateVars name newValue (Node n v left right)
  | name < n  = Node n v (updateVars name newValue left) right
  | name > n  = Node n v left (updateVars name newValue right)
  | otherwise = Node name newValue left right  -- Replace existing value
-- filter is a Prelude function that takes a function and a list
-- and returns the elements of the list that satisfy that condition

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name vars = filter (\(x, _) -> x /= name) vars

updateLastResult :: Maybe Value -> REPLState -> REPLState
updateLastResult v st = st {lastResult = v}

-- Add a command to the command history in the state
addHistory :: REPLState -> Command -> REPLState
addHistory st cmd = st {history = history st ++ [cmd]} 

process :: REPLState -> Command -> IO ()
process st (Set var e) = 
  case eval (vars st) e of
    Just v -> do
      putStrLn "OK"
      let newVars = updateVars var v (vars st)
          newSt = addHistory st (Set var e)  -- Add to history
          finalSt = newSt { vars = newVars, lastResult = Just v }
      repl finalSt
    Nothing -> do
      putStrLn "Evaluation error!"
      repl st

-- repl print result when evluating expression         
process st (Eval e) = 
  case eval (vars st) e of
    Just v -> do
      print v
      let newSt = addHistory st (Eval e)
          updatedVars = updateVars "it" v (vars newSt)
          finalSt = newSt { vars = updatedVars, lastResult = Just v }
      repl finalSt
    Nothing -> do
      putStrLn "Evaluation error!"
      repl st 

process st (History n) = 
  if n >= 0 && n < length (history st)
  then do
    putStrLn $ "command " ++ show n ++ " : " ++ show (history st !! n)
    process st (history st !! n) 
  else do
    putStrLn "Invalid command number"
    repl st


-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: REPLState -> IO ()
repl st = do
  putStr $ show (length (history st)) ++ " > "
  inp <- getLine
  case parse pCommand inp of
    [(Quit, "")] -> putStrLn "Bye"
    [(cmd, "")] -> process st cmd
    _ -> do
      putStrLn "Parse error"
      repl st

