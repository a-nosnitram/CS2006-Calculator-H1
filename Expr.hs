module Expr where

import Parsing

type Name = String

data Value = VInt Int | VFloat Double deriving (Eq)


instance Show Value where
  show (VInt x) = show x
  show (VFloat x) 
    | x == fromIntegral (floor x) = show (floor x) -- shows as int for whole numbers
    | otherwise = show x -- float

-- At first, 'Expr' contains only addition and values. You will need to 
-- add other operations, and variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Name
          | Val Value -- literal value (int or float)          
       --   | StrVal String
       --   | Concat Expr Expr 
  deriving (Show, Eq)


-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
             | Eval Expr
             | Quit --allows quit
             | History Int -- allows getting command history
             | Clear -- clears history
             | Comment String -- for commenting 
             | EmptyLine -- to ignore empty lines 
             | Loop Int [Command] -- for loops 
             | Print Command -- for printing
  deriving (Show, Eq)

data VarTree = Empty
             | Node Name Value VarTree VarTree
  deriving (Show, Eq)

lookupVar :: Name -> VarTree -> Maybe Value
lookupVar _ Empty = Nothing
lookupVar name (Node n v left right)
  | name == n  = Just v
  | name < n   = lookupVar name left
  | otherwise  = lookupVar name right

-- evaluator function for epressions
eval :: VarTree -> Expr -> Maybe Value
-- Handle numeric values directly
eval _ (Val v) = Just v
-- Handle variables (look up their value in the variable list)
eval vars (Var x) = lookupVar x vars

--Add--------------------
eval vars (Add x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VInt (a + b)
    (VFloat a, VFloat b) -> Just $ VFloat (a + b)
    (VInt a, VFloat b)   -> Just $ VFloat (fromIntegral a + b)
    (VFloat a, VInt b)   -> Just $ VFloat (a + fromIntegral b)

--Sub--------------------
eval vars (Sub x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VInt (a - b)
    (VFloat a, VFloat b) -> Just $ VFloat (a - b)
    (VInt a, VFloat b)   -> Just $ VFloat (fromIntegral a - b)
    (VFloat a, VInt b)   -> Just $ VFloat (a - fromIntegral b)

--Mul-------------------
eval vars (Mul x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VInt (a * b)
    (VFloat a, VFloat b) -> Just $ VFloat (a * b)
    (VInt a, VFloat b)   -> Just $ VFloat (fromIntegral a * b)
    (VFloat a, VInt b)   -> Just $ VFloat (a * fromIntegral b)

--Div-------------------
eval vars (Div x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (_, VInt 0)        -> Nothing
    (_, VFloat 0.0)    -> Nothing
    (VInt a, VInt b)   -> Just $ VFloat (fromIntegral a / fromIntegral b)
    (VFloat a, VFloat b) -> Just $ VFloat (a / b)
    (VInt a, VFloat b) -> Just $ VFloat (fromIntegral a / b)
    (VFloat a, VInt b) -> Just $ VFloat (a / fromIntegral b)

--Concat-----------------
{- eval vars (Concat x y) = do 
   x' <- eval vars x
   y' <- eval vars y
   return (x' ++ y') -}

-- This is a helper function to convert char to int
digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

-- This is a function for converting multiple digits (strings of digits) to integers
-- stringToInt :: String -> Int 
-- stringToInt ns = foldl (\a x -> a * 10 + digitToInt x) 0 ns

-- added token to ignore leading and trailing spaces
pCommand :: Parser Command
pCommand = do
              space
              cmd <- command -- parsing main command 
              many comment
              return cmd
           ||| do comment
           ||| do emptyLine     

  where 
    command = do 
           t <- identifier
           symbol "="
           e <- pExpr
           return (Set t e)
       ||| token (do 
            e <- pExpr
            return (Eval e))
       ||| token (do 
            string ":q" -- allows quit
            return Quit) -- allows quit
       ||| token (do 
            string ":c"
            return Clear) -- allows to clear history
       ||| token (do 
            char ':'  -- command history 
            ns <- many1 digit -- multiple digits
            return (History (read ns)))
       ||| token (do 
            string ":print"
            cmd <- command
            return (Print cmd))
       ||| token (do 
            string ":loop"
            space
            ns <- many1 digit 
            space
            char '['
            space
            cmds <- parseCommands 
            space
            char ']'
            space
            return (Loop (read ns) cmds))

    comment = token (do 
         char '#'
         comment <- many (sat (\x -> x /= '\n'))
         return (Comment comment))

    emptyLine = token (do 
                many (sat (\x -> x `elem` " \t"))
                return EmptyLine)  
    
    parseCommands :: Parser [Command]
    parseCommands = do 
       first <- command
       rest <- many (do 
            space
            char ';'
            space
            command)
       return (first:rest)

-- This is the expression parser
-- for add and sub
pExpr :: Parser Expr
pExpr = do t <- pTerm
           (do symbol "+"
               e <- pExpr
               return (Add t e)
            ||| do symbol "-"
                   e <- pExpr
                   return (Sub t e)
            ||| return t)


-- This is the factor parser with 
-- added multi-digit support
-- and negative number support
pFactor :: Parser Expr
pFactor = do d <- double --double parsed first to catch decimal oints
             return (Val (VFloat d))
        ||| do n <- integer
               return (Val (VInt n))
        ||| do v <- identifier
               return (Var v)
        ||| do symbol "("
               e <- pExpr
               symbol ")"
               return e

-- This is the term parser for mul and div
pTerm :: Parser Expr
pTerm = do f <- pFactor
           (do symbol "*"
               t <- pTerm
               return (Mul f t)
            ||| do symbol "/"
                   t <- pTerm
                   return (Div f t)
            ||| return f)

-- This parses strings 
{- pStrVal :: Parser Expr 
pStrVal = do 
   char '"'
   s <- many (sat (\x -> x /= '"'))
   char '"'
   return (StrVal s) -} 
