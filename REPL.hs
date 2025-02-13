module REPL where

import Expr
import Parsing

-- In REPL.hs
data REPLState = REPLState { 
  vars :: [(Name, Value)], 
  history :: [Command], 
  printHis :: [String], -- printable history 
  lastResult :: Maybe Value  
}

initREPLState :: REPLState
initREPLState = REPLState [] [] [] Nothing

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
updateVars name newValue vars = (name, newValue) : filter (\(x, _) -> x /= name) vars 
-- filter is a Prelude function that takes a function and a list
-- and returns the elements of the list that satisfy that condition

-- Return a new set of variables with the given name removed
dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name vars = filter (\(x, _) -> x /= name) vars

updateLastResult :: Maybe Value -> REPLState -> REPLState
updateLastResult v st = st {lastResult = v}

-- Add a command to the command history in the state
addHistory :: REPLState -> Command -> String -> REPLState
addHistory st cmd cmdp = st {history = history st ++ [cmd], printHis = printHis st ++ [cmdp]} 

process :: REPLState -> Command -> String -> IO ()
process st Quit inp = putStrLn "Bye"

process st (Set var e) inp = 
  case eval (vars st) e of
    Just v -> do
      putStrLn ("OK : " ++ var ++ " = " ++ show v)
      let newVars = updateVars var v (vars st)
          newSt = addHistory st (Set var e) inp  -- Add to history
          finalSt = newSt { vars = newVars, lastResult = Just v }
      repl finalSt
    Nothing -> do
      putStrLn "Evaluation error!"
      repl st

-- repl print result when evluating expression         
process st (Eval e) inp = 
  case eval (vars st) e of
    Just v -> do
      print v
      let newSt = addHistory st (Eval e) inp
          updatedVars = updateVars "it" v (vars newSt)
          finalSt = newSt { vars = updatedVars, lastResult = Just v }
      repl finalSt
    Nothing -> do
      putStrLn "Evaluation error!"
      repl st 

process st (History n) inp = 
  if n >= 0 && n < length (history st)
  then do
    putStrLn $ "command " ++ show n ++ " : " ++ (printHis st !! n)
    process st (history st !! n) (printHis st !! n) 
  else do
    putStrLn "Invalid command number"
    repl st

process st (Clear) inp = do 
        repl st {history = [], printHis = []}

process st (Comment _) inp = do
        repl st -- do nothing / ignore comments

process st (EmptyLine) inp = do 
        repl st

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'processFileLine to process the command.
-- 'processFileLine will call 'repl' when done, so the system loops.

repl :: REPLState -> IO ()
repl st = do
  putStr $ show (length (history st)) ++ " > "
  inp <- getLine
  case parse pCommand inp of
    [(cmd, "")] -> process st cmd inp
    _ -> do
      putStrLn "Parse error"
      repl st

-- processFileLine has a IO REPLState return type 
-- it is a version of process used when reading a file 
processFileLine :: REPLState -> Command -> IO REPLState
processFileLine st (Set var e) = 
  case eval (vars st) e of
    Just v -> do
      let newVars = updateVars var v (vars st)
          newSt = addHistory st (Set var e) ""  -- Add to history
          finalSt = newSt { vars = newVars, lastResult = Just v }
      return finalSt
    Nothing -> do
      putStrLn ("Evaluation error at command number " ++ show (length (history st)) ++ " !")     
      return st

processFileLine st (Eval e) = 
  case eval (vars st) e of
    Just v -> do
      let newSt = addHistory st (Eval e) ""
          updatedVars = updateVars "it" v (vars newSt)
          finalSt = newSt { vars = updatedVars, lastResult = Just v }
      return finalSt
    Nothing -> do
      putStrLn ("Evaluation error at command number " ++ show (length (history st)) ++ " !")
      return st 

processFileLine st (Print command) = do 
        -- processFileLine st (command)
        case command of
          Eval e -> case eval (vars st) e of 
                         Just v  -> do
                              print v 
                         Nothing -> do
                              putStrLn ("Evaluation error at command number " ++ show (length (history st)) ++ " !")
                    return processFileLine st (command)
          Set var e -> case 

processFileLine st (Quit) = do 
        putStrLn "error : Quit command is not allowed within a file"

processFileLine st (Clear) = do 
        putStrLn "error : Clear command is not allowed within a file"

processFileLine st (History n) = do 
        putStrLn "error : History command is not allowed within a file"

processFileLine st (Comment _) = do
        return st -- do nothing / ignore comments

processFileLine st (EmptyLine) = do
        return st
