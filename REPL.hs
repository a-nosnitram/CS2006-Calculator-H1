module REPL where

import Expr
import Expr (VarTree, lookupVar)
import Parsing

-- Data type representing the REPL state
data REPLState = REPLState { 
  vars :: Expr.VarTree,  -- Variable tree storing name-value pairs
  history :: [Command],  -- Command execution history
  printHis :: [String],  -- Printable command history
  lastResult :: Maybe Value  -- Last computed result
} deriving (Show, Eq)

-- Initial REPL state with empty history and variables
initREPLState :: REPLState
initREPLState = REPLState Empty [] [] Nothing

-- Update variable tree by adding or replacing a variable
-- If the variable already exists, its value is updated 
updateVars name newValue Empty = Node name newValue Empty Empty
updateVars name newValue (Node n v left right)
  | name < n  = Node n v (updateVars name newValue left) right
  | name > n  = Node n v left (updateVars name newValue right)
  | otherwise = Node name newValue left right
-- filter is a Prelude function that takes a function and a list
-- and returns the elements of the list that satisfy that condition

-- Remove a variable from the list of variables
dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar name vars = filter (\(x, _) -> x /= name) vars

-- Update the last computed result in the REPL state
updateLastResult :: Maybe Value -> REPLState -> REPLState
updateLastResult v st = st {lastResult = v}

-- Add a command to the command history
addHistory :: REPLState -> Command -> String -> REPLState
addHistory st cmd cmdp = st {history = history st ++ [cmd], printHis = printHis st ++ [cmdp]} 

-- Process commands in the REPL 
process :: REPLState -> Command -> String -> IO ()
process st Quit inp = putStrLn "Bye"

-- Process a 'Set' command: evaluates an expression and updates variables
process st (Set var e) inp = 
  if var == "it" then do
    putStrLn "Error: 'it' is a reserved variable name"
    repl st
  else
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

-- Process an 'Eval' command: evaluates an expression and stores it as 'it'
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

-- Process a 'Simplify' command
process st (Simplify e) inp = do
  let simplified = simplifyExpr e
  putStrLn (show simplified)
  repl st 

-- Process a 'History' command: retrieves and re-executes a command
process st (History n) inp = 
  if n >= 0 && n < length (history st)
  then do
    putStrLn $ "command " ++ show n ++ " : " ++ (printHis st !! n)
    process st (history st !! n) (printHis st !! n) 
  else do
    putStrLn "Error: Invalid command number"
    repl st

-- Process a 'Loop' command: executes a sequence of commands n times
process st (Loop n commands) inp = do
        let newSt = addHistory st (Loop n commands) inp 
        if n == 0 then repl newSt 
        else if n < 0 then do
                putStrLn "Error: Invalid loop syntax"
                repl st
        else do  
                st' <- processFileLine newSt (Loop n commands) (-1)
                repl st'

-- Clears command history
process st (Clear) inp = do 
        repl st {history = [], printHis = []}

-- Ignore comments in the REPL
process st (Comment _) inp = do
        repl st 

-- Ignore empty lines in the REPL
process st (EmptyLine) inp = do 
        repl st 

-- Process a 'Print' command: evaluates expressions and prints results
process st (Print exprs) inp = do 
        let newSt = addHistory st (Print exprs) inp
        st' <- processFileLine newSt (Print exprs) (-1)
        repl st'

-- The main Read-Eval-Print Loop
repl :: REPLState -> IO ()
repl st = do
  putStr $ show (length (history st)) ++ " > "
  inp <- getLine
  case parse pCommand inp of
    [(cmd, "")] -> process st cmd inp
    _ -> do
      putStrLn "Parse Error"
      repl st

-- Processes commands from a file, returning a new REPL state
processFileLine :: REPLState -> Command -> Int -> IO REPLState
processFileLine st (Set var e) l = 
  if var == "it" then do
    if l == (-1) then 
      putStrLn "Error: 'it' is a reserved variable name"
    else 
      putStrLn $ "Error at line " ++ show l ++ ": 'it' is a reserved variable name"
    return st
  else
    case eval (vars st) e of
      Right v -> do
        let newVars = updateVars var v (vars st)
            finalSt = st { vars = newVars, lastResult = Just v }
        return finalSt
      Left err -> do
        if l == (-1) then 
             putStrLn $ "Error: " ++ err
        else 
             putStrLn $ "Error at line " ++ show l ++ ": " ++ err
        return st

processFileLine st (Eval e) l = 
  case eval (vars st) e of
    Right v -> do
      let updatedVars = updateVars "it" v (vars st)
          finalSt = st { vars = updatedVars, lastResult = Just v }
      return finalSt
    Left err -> do
      if l == (-1) then 
            putStrLn $ "Error: " ++ err
      else 
            putStrLn $ "Error at line " ++ show l ++ ": " ++ err
      return st 

processFileLine st (Print exprs) l = do  
        st' <- processExprs st exprs l 
        return st'

processFileLine st (Quit) l = do
        putStrLn $ "Error at line " ++ show l ++ ": Quit command is not allowed within a file"
        return st

processFileLine st (Clear) l = do
        putStrLn $ "Error at line " ++ show l ++ ": Clear command is not allowed within a file"
        return st

processFileLine st (History n) l = do 
        putStrLn $ "Error at line " ++ show l ++ ": History command is not allowed within a file"
        return st

processFileLine st (Simplify e) l = do
  let simplified = simplifyExpr e
  putStrLn (">> " ++ show simplified)
  return st



processFileLine st (Loop n commands) l =
        if n < 0 then do
               if l == (-1) then 
                     putStrLn ("Error: Invalid loop syntax")
               else 
                     putStrLn ("Error at line " ++ show l ++ ": Invalid loop syntax")
               return st
        else if n == 0 then return st 
        else do  
               st' <- processCommands st commands l        
               if lastResult st' == Nothing then -- check if error occured 
                   return st' -- break loop, if so 
               else 
                   processFileLine st' (Loop (n-1) commands) l 

processFileLine st (Comment _) l = do
        return st 

processFileLine st (EmptyLine) l = do
        return st

-- Processes multiple expressions for the :print command
processExprs :: REPLState -> [Expr] -> Int -> IO REPLState
processExprs st [] _ = return st 
processExprs st (expr:exprs) l = 
        case eval (vars st) expr of 
                Right v -> do putStrLn (">> " ++ show v)
                              let updatedVars = updateVars "it" v (vars st)
                                  finalSt = st { vars = updatedVars, lastResult = Just v }
                              processExprs finalSt exprs l
                Left err -> do if l == (-1) then 
                                   putStrLn ("Error: " ++ err)
                               else
                                   putStrLn ("Error at line " ++ show l ++ ": " ++ err)
                               return st { lastResult = Nothing } -- will break the processing 

-- Process multiple commands within a :loop
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
             Print exprs -> do
                st' <- processFileLine st cmd l
                processCommands st' cmds l
             Loop n commands -> do 
                st' <- processFileLine st cmd l
                processCommands st' cmds l
             _ -> do 
                if l == (-1) then 
                    putStrLn "Error: This command cannot be looped" 
                else 
                    putStrLn ("Error at line " ++ show l ++ ": This command cannot be looped")
                return st { lastResult = Nothing } -- this will break the loop 
