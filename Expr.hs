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

pCommand :: Parser Command
pCommand = do t <- letter
              char '='
              e <- pExpr
              return (Set [t] e)
            ||| do e <- pExpr
                   return (Eval e)
            ||| do string ":q" -- allows quit
                   return Quit -- allows quit
            ||| do char ':'  -- command history 
	           ns <- many digit -- multiple digits
		   return (History (stringToInt ns))

pExpr :: Parser Expr
pExpr = do t <- pTerm
           do char '+'
              e <- pExpr
              return (Add t e)
            ||| do char '-'
                   e <- pExpr
                   return (Sub t e)
            ||| return t


pFactor :: Parser Expr
pFactor = do d <- digit
             return (Val (digitToInt d))
           ||| do v <- letter
                  return (Var [v])
                ||| do char '('
                       e <- pExpr
                       char ')'
                       return e

pTerm :: Parser Expr
pTerm = do f <- pFactor
           do char '*'
              t <- pTerm
              return (Mul f t)
            ||| do char '/'
                   t <- pTerm
                   return (Div f t)
            ||| return f



