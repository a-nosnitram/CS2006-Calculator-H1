module REPL where

import Expr
import Expr (VarTree, lookupVar)
import Parsing

-- In REPL.hs
data REPLState = REPLState { 
  -- vars :: [(Name, Value)], 
  vars :: Expr.VarTree,
  history :: [Command], 
  printHis :: [String], -- printable history 
  lastResult :: Maybe Value  
}

initREPLState :: REPLState

-- initREPLState = REPLState [] [] Nothing
initREPLState = REPLState Empty [] [] Nothing

-- Given a variable name and a value, return a new set of variables with
-- that name and value added.
-- If it already exists, remove the old value
updateVars :: Name -> Value -> VarTree -> VarTree
updateVars name newValue Empty = Node name newValue Empty Empty
updateVars name newValue (Node n v left right)
  | name < n  = Node n v (updateVars name newValue left) right
  | name > n  = Node n v left (updateVars name newValue right)
  | otherwise = Node name newValue left right
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
    putStrLn "error : Invalid command number"
    repl st

--process st (Loop n command) inp = do
--        if n == 0 then do 
--		repl st 
--        else if n < 0 then do
--           putStrLn "error : Invalid loop syntax"
--           repl st
--        else do
--           loopHelper n st command inp

process st (Clear) inp = do 
        repl st {history = [], printHis = []}

process st (Comment _) inp = do
        repl st -- do nothing / ignore comments

process st (EmptyLine) inp = do 
        repl st

--loopHelper :: Int -> REPLState -> Command -> String -> IO ()
--loopHelper n st command inp = 
--   if n == 0 then return ()
--   else do 
--        process st command inp 
--        loopHelper (n-1) st command inp

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

-- processFileLine has a IO REPLState return type 
-- it is a version of process used when reading a file 
processFileLine :: REPLState -> Command -> Int -> IO REPLState
processFileLine st (Set var e) l = 
  case eval (vars st) e of
    Just v -> do
      let newVars = updateVars var v (vars st)
          newSt = addHistory st (Set var e) ""  -- Add to history
          finalSt = newSt { vars = newVars, lastResult = Just v }
      return finalSt
    Nothing -> do
      putStrLn ("error : Evaluation falied at line " ++ show l)
      return st

processFileLine st (Eval e) l = 
  case eval (vars st) e of
    Just v -> do
      let newSt = addHistory st (Eval e) ""
          updatedVars = updateVars "it" v (vars newSt)
          finalSt = newSt { vars = updatedVars, lastResult = Just v }
      return finalSt
    Nothing -> do
      putStrLn ("error : Evaluation falied at line " ++ show l)
      return st 

processFileLine st (Print command) l = do  
        case command of 
           Eval e -> case eval (vars st) e of 
             Just v -> putStrLn (">> " ++ show v)
             Nothing -> putStrLn ("Evaluation error at command number " ++ show (length (history st)) ++ " !")
           Set var e -> case eval (vars st) e of 
             Just v -> putStrLn ("OK : " ++ var ++ " = " ++ show v)
             Nothing -> putStrLn ("Evaluation error at command number " ++ show (length (history st)) ++ " !")
           _ -> putStrLn ("error at line " ++ show l ++ ": You cannot print this command. only evaluations and set commands can be printed.")
        return st

processFileLine st (Quit) l = do 
        putStrLn ("error at line " ++ show l ++ ": Quit command is not allowed within a file")
        return st

processFileLine st (Clear) l = do 
        putStrLn ("error at line " ++ show l ++ ": Clear command is not allowed within a file")
        return st

processFileLine st (History n) l = do 
        putStrLn ("error at line " ++ show l ++ ": History command is not allowed within a file")
        return st

processFileLine st (Loop n command) l =
        if n < 0 then do
               putStrLn ("error at line " ++ show l ++ ": invalid loop syntax")
               return st
        else if n == 0 then return st 
        else do  
               st' <- processFileLine st command l
               processFileLine st' (Loop (n-1) command) l

processFileLine st (Comment _) l = do
        return st -- do nothing / ignore comments

processFileLine st (EmptyLine) l = do
        return st
