module Expr where

import Parsing

type Name = String

-- At first, 'Expr' contains only addition and values. You will need to 
-- add other operations, and variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Name
          | Val Int
  deriving Show


-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
             | Eval Expr
             | Quit --allows quit
             | History Int -- allows getting command history
  deriving Show



eval :: [(Name, Int)] -> Expr -> Maybe Int
-- Handle numeric values directly
eval _ (Val x) = Just x

-- Handle variables (look up their value in the variable list)
eval vars (Var x) = lookup x vars

--eval :: [(Name, Int)] -> -- Variable name to value mapping
--        Expr -> -- Expression to evaluate
--        Maybe Int -- Result (if no errors such as missing variables)
--eval vars (Val x) = Just x -- for values, just give the value directly

--eval vars (Add x y) = Nothing -- return an error (because it's not implemented yet!)

--Add--------------------
eval vars (Add x y) = do
  x' <- eval vars x
  y' <- eval vars y
  return (x' + y')
--Sub--------------------
eval vars (Sub x y) = do
  x' <- eval vars x
  y' <- eval vars y
  return (x' - y')
  
--Mul-------------------
eval vars (Mul x y) = do
  x' <- eval vars x
  y' <- eval vars y
  return (x' * y')
--Div-------------------
eval vars (Div x y) = do
  x' <- eval vars x
  y' <- eval vars y
  if y' == 0 then Nothing else return (x' `div` y')


digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

-- function for converting multiple digits int integers
stringToInt :: String -> Int 
stringToInt ns = foldl (\a x -> a * 10 + digitToInt x) 0 ns

-- added token to ignore leading and trailing spaces
pCommand :: Parser Command
pCommand = do t <- identifier
              symbol "="
              e <- pExpr
              return (Set t e)
            ||| token (do e <- pExpr
                          return (Eval e))
            ||| token (do string ":q" -- allows quit
                          return Quit) -- allows quit
            ||| token (do char ':'  -- command history 
                          ns <- many1 digit -- multiple digits
                          return (History (stringToInt ns)))

pExpr :: Parser Expr
pExpr = do t <- pTerm
           (do symbol "+"
               e <- pExpr
               return (Add t e)
            ||| do symbol "-"
                   e <- pExpr
                   return (Sub t e)
            ||| return t)


-- added multi-digit support
-- neg numbers
pFactor :: Parser Expr
pFactor = do n <- integer
             return (Val n)
           ||| do v <- identifier
                  return (Var v)
           ||| do symbol "("
                  e <- pExpr
                  symbol ")"
                  return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           (do symbol "*"
               t <- pTerm
               return (Mul f t)
            ||| do symbol "/"
                   t <- pTerm
                   return (Div f t)
            ||| return f)



