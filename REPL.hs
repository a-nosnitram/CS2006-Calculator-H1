module REPL where

import Expr
import Expr (VarTree, lookupVar)
import Parsing

-- In REPL.hs
data REPLState = REPLState { 
  vars :: Expr.VarTree,
  history :: [Command], 
  printHis :: [String], -- printable history 
  lastResult :: Maybe Value  
} deriving (Show, Eq)

initREPLState :: REPLState
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

-- This function adds a command to the command history in the state
addHistory :: REPLState -> Command -> String -> REPLState
addHistory st cmd cmdp = st {history = history st ++ [cmd], printHis = printHis st ++ [cmdp]} 

-- This function processes commands one by one 
process :: REPLState -> Command -> String -> IO ()
process st Quit inp = putStrLn "Bye"

process st (Set var e) inp = 
  case eval (vars st) e of
    Right v -> do
      putStrLn ("OK : " ++ var ++ " = " ++ show v)
      let newVars = updateVars var v (vars st)
          newSt = addHistory st (Set var e) inp  -- Add to history
          finalSt = newSt { vars = newVars, lastResult = Just v }
      repl finalSt
    Left err -> do
      putStrLn $ "Error: " ++ err
      repl st

process st (Eval e) inp = 
  case eval (vars st) e of
    Right v -> do
      print v
      let newSt = addHistory st (Eval e) inp
          updatedVars = updateVars "it" v (vars newSt)
          finalSt = newSt { vars = updatedVars, lastResult = Just v }
      repl finalSt
    Left err -> do
      putStrLn $ "Error: " ++ err
      repl st 

process st (History n) inp = 
  if n >= 0 && n < length (history st)
  then do
    putStrLn $ "command " ++ show n ++ " : " ++ (printHis st !! n)
    process st (history st !! n) (printHis st !! n) 
  else do
    putStrLn "error : Invalid command number"
    repl st

process st (Loop n commands) inp = do
        let newSt = addHistory st (Loop n commands) inp 
        if n == 0 then repl newSt 
        else if n < 0 then do
                putStrLn "error : Invalid loop syntax"
                repl st
        else do  
                st' <- processFileLine newSt (Loop n commands) (-1)
                repl st'

process st (Clear) inp = do 
        repl st {history = [], printHis = []}

process st (Comment _) inp = do
        repl st -- do nothing / ignore comments

process st (EmptyLine) inp = do 
        repl st -- do nothing / ignore empty lines

process st (Print command) inp =
        case command of 
             Eval e -> case eval (vars st) e of 
                  Just v -> do 
                         putStrLn (">>> " ++ show v)
                         let newSt = addHistory st (Eval e) inp
                             updatedVars = updateVars "it" v (vars newSt)
                             finalSt = newSt { vars = updatedVars, lastResult = Just v }
                         repl finalSt 
                  Nothing -> putStrLn ("error : Evaluation failed") 
             _ -> do putStrLn ("error : You cannot print this command.")
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

-- processFileLine has a IO REPLState return type 
-- it is a version of process used when reading a file 
processFileLine :: REPLState -> Command -> Int -> IO REPLState
processFileLine st (Set var e) l = 
  case eval (vars st) e of
    Right v -> do
      let newVars = updateVars var v (vars st)
          finalSt = st { vars = newVars, lastResult = Just v }
      return finalSt
    Left err -> do
      if l == (-1) then 
           putStrLn ("error : Evaluation failed")
      else 
           putStrLn ("error at line " ++ show l ++ ": Evaluation falied")
      return st

processFileLine st (Eval e) l = 
  case eval (vars st) e of
    Right v -> do
      let updatedVars = updateVars "it" v (vars st)
          finalSt = st { vars = updatedVars, lastResult = Just v }
      return finalSt
    Left err -> do
      if l == (-1) then 
            putStrLn ("error : Evaluation failed")
      else 
            putStrLn ("error at line " ++ show l ++ ": Evaluation falied")
      return st 

processFileLine st (Print command) l = 
        case command of 
             Eval e -> case eval (vars st) e of 
                  Right v -> do putStrLn (">> " ++ show v)
                                let newSt = addHistory st (Eval e) "This command is not a standalone, but part of a loop"
                                    updatedVars = updateVars "it" v (vars newSt)
                                    finalSt = newSt { vars = updatedVars, lastResult = Just v }
                                 return finalSt
                  Left err -> do putStrLn ("error at line " ++ show l ++ ": Evaluation failed")
                                 return st
             _ -> do putStrLn ("error at line " ++ show l ++ ": You cannot print this command")
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


processFileLine st (Loop n commands) l =
        if n < 0 then do
               if l == (-1) then 
                     putStrLn ("error : invalid loop syntax")
               else 
                     putStrLn ("error at line " ++ show l ++ ": invalid loop syntax")
               return st
        else if n == 0 then return st 
        else do  
               st' <- processCommands st commands l
               if st' == st then 
                   return st
               else 
                   processFileLine st' (Loop (n-1) commands) l

processFileLine st (Comment _) l = do
        return st -- do nothing / ignore comments

processFileLine st (EmptyLine) l = do
        return st

processCommands :: REPLState -> [Command] -> Int -> IO REPLState
processCommands st [] _ = return st
processCommands st (cmd:cmds) l = 
    case cmd of 
             Eval e -> do    
                st' <- processFileLine st cmd l
                processCommands st' cmds l
             Set vars e -> do
                st' <- processFileLine st cmd l
                processCommands st' cmds l
             Print command -> do
                st' <- processFileLine st cmd l
                processCommands st' cmds l
             Loop n commands -> do 
                st' <- processFileLine st cmd l
                processCommands st' cmds l
             _ -> do 
                if l == (-1) then 
                    putStrLn "error : this command cannot be looped" 
                else 
                    putStrLn ("error at line " ++ show l ++ ": this command cannot be looped")
                return st 
