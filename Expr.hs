module Expr where

import Parsing
import Data.Fixed (mod')

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
          | Power Expr Expr
          | Mod Expr Expr
          | Abs Expr
  deriving Show


-- These are the REPL commands - set a variable name to a value, and evaluate
-- an expression
data Command = Set Name Expr
             | Eval Expr
             | Quit --allows quit
             | History Int -- allows getting command history
  deriving Show

data VarTree = Empty
             | Node Name Value VarTree VarTree

lookupVar :: Name -> VarTree -> Maybe Value
lookupVar _ Empty = Nothing
lookupVar name (Node n v left right)
  | name == n  = Just v
  | name < n   = lookupVar name left
  | otherwise  = lookupVar name right

-- evaluator function for epressions
-- eval :: [(Name, Value)] -> Expr -> Maybe Value
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

eval vars (Power x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VInt (a ^ b)
    (VFloat a, VFloat b) -> Just $ VFloat (a ** b)
    (VInt a, VFloat b)   -> Just $ VFloat (fromIntegral a ** b)
    (VFloat a, VInt b)   -> Just $ VFloat (a ** fromIntegral b)

eval vars (Mod x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (_, VInt 0)        -> Nothing
    (_, VFloat 0.0)    -> Nothing
    (VInt a, VInt b)   -> Just $ VInt (a `mod` b)
    (VFloat a, VFloat b) -> Just $ VFloat (a `mod'` b)
    (VInt a, VFloat b) -> Just $ VFloat (fromIntegral a `mod'` b)
    (VFloat a, VInt b) -> Just $ VFloat (a `mod'` fromIntegral b)

eval vars (Abs x) = do
  x' <- eval vars x
  case x' of
    VInt a   -> Just $ VInt (abs a)
    VFloat a -> Just $ VFloat (abs a)


--helper function to convert char to int
digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

-- function for converting multiple digits(string of digits)to integers
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

--expression parser
--for add and sub
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

--term parser (mul and div)
pTerm :: Parser Expr
pTerm = do symbol "abs"
           symbol "("
           e <- pExpr
           symbol ")"
           return (Abs e)
        ||| do f <- pFactor
               (do symbol "*"
                   t <- pTerm
                   return (Mul f t)
                ||| do symbol "/"
                       t <- pTerm
                       return (Div f t)
                ||| do symbol "^"
                       t <- pTerm
                       return (Power f t)
                ||| do symbol "%"
                       t <- pTerm
                       return (Mod f t)
                ||| return f)

-- Existing parser for factors like numbers and variables



