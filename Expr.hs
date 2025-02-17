module Expr where

import Parsing
import Data.Fixed (mod')
import Control.Applicative hiding (many)

type Name = String

-- Data type for representing values
data Value = VInt Int | VFloat Double | VBool Bool deriving (Eq)


instance Show Value where
  show (VInt x) = show x
  show (VFloat x) 
    | x == fromIntegral (floor x) = show (floor x) -- shows as int for whole numbers
    | otherwise = show x -- float
  show (VBool b) = show b

--Expression datatypes, representing various operations
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Var Name
          | Val Value -- literal value (int or float)
          | Power Expr Expr
          | Mod Expr Expr
          | Abs Expr
          | Sin Expr
          | Cos Expr
          | Tan Expr
          --boolean and comparason operations
          | And Expr Expr
          | Or Expr Expr
          | Not Expr
          | Eq Expr Expr    -- ==
          | Neq Expr Expr   -- !=
          | Lt Expr Expr    -- <
          | Gt Expr Expr    -- >
          | Leq Expr Expr   -- <=
          | Geq Expr Expr   -- >=
  deriving Show


--Commands for REPL
data Command = Set Name Expr
             | Eval Expr
             | Quit --allows quit
             | History Int -- get command history
             | Clear -- clears history
             | Comment String -- for commenting 
             | EmptyLine -- to ignore empty lines 
             | Loop Int Command -- for loops (not yet implemented)
             | Print Command
  deriving Show

-- bianary search tree for storing variables
data VarTree = Empty
             | Node Name Value VarTree VarTree

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

--Addition--------------------
eval vars (Add x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VInt (a + b)
    (VFloat a, VFloat b) -> Just $ VFloat (a + b)
    (VInt a, VFloat b)   -> Just $ VFloat (fromIntegral a + b)
    (VFloat a, VInt b)   -> Just $ VFloat (a + fromIntegral b)

--Subtraction--------------------
eval vars (Sub x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VInt (a - b)
    (VFloat a, VFloat b) -> Just $ VFloat (a - b)
    (VInt a, VFloat b)   -> Just $ VFloat (fromIntegral a - b)
    (VFloat a, VInt b)   -> Just $ VFloat (a - fromIntegral b)

--Multiplication-------------------
eval vars (Mul x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VInt (a * b)
    (VFloat a, VFloat b) -> Just $ VFloat (a * b)
    (VInt a, VFloat b)   -> Just $ VFloat (fromIntegral a * b)
    (VFloat a, VInt b)   -> Just $ VFloat (a * fromIntegral b)

--Division-------------------
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

--Power (exponentiation)-----------------
eval vars (Power x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VInt (a ^ b)
    (VFloat a, VFloat b) -> Just $ VFloat (a ** b)
    (VInt a, VFloat b)   -> Just $ VFloat (fromIntegral a ** b)
    (VFloat a, VInt b)   -> Just $ VFloat (a ** fromIntegral b)

--Modulo-------------------------
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

--Absolute Value------------------------
eval vars (Abs x) = do
  x' <- eval vars x
  case x' of
    VInt a   -> Just $ VInt (abs a)
    VFloat a -> Just $ VFloat (abs a)

--Trigonometric functions----------------
--sine
eval vars (Sin x) = do
  x' <- eval vars x
  case x' of
    VInt a   -> Just $ VFloat (sin (fromIntegral a))
    VFloat a -> Just $ VFloat (sin a)

--cosine
eval vars (Cos x) = do
  x' <- eval vars x
  case x' of
    VInt a   -> Just $ VFloat (cos (fromIntegral a))
    VFloat a -> Just $ VFloat (cos a)

--tangent
eval vars (Tan x) = do
  x' <- eval vars x
  case x' of
    VInt a   -> Just $ VFloat (tan (fromIntegral a))
    VFloat a -> Just $ VFloat (tan a)

--boolean operators-------------------------------
--And (&&)
eval vars (And x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VBool a, VBool b) -> Just $ VBool (a && b)
    _ -> Nothing

--Or (||)
eval vars (Or x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VBool a, VBool b) -> Just $ VBool (a || b)
    _ -> Nothing

--Not
eval vars (Not x) = do
  x' <- eval vars x
  case x' of
    VBool a -> Just $ VBool (not a)
    _ -> Nothing

--Comparason operators--------
--Equality (==)
eval vars (Eq x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VBool (a == b)
    (VFloat a, VFloat b) -> Just $ VBool (a == b)
    (VInt a, VFloat b)   -> Just $ VBool (fromIntegral a == b)
    (VFloat a, VInt b)   -> Just $ VBool (a == fromIntegral b)
    (VBool a, VBool b)   -> Just $ VBool (a == b)
    _ -> Nothing

--inEquality (/=)
eval vars (Neq x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VBool (a /= b)
    (VFloat a, VFloat b) -> Just $ VBool (a /= b)
    (VInt a, VFloat b)   -> Just $ VBool (fromIntegral a /= b)
    (VFloat a, VInt b)   -> Just $ VBool (a /= fromIntegral b)
    (VBool a, VBool b)   -> Just $ VBool (a /= b)
    _ -> Nothing  

--Less Than
eval vars (Lt x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VBool (a < b)
    (VFloat a, VFloat b) -> Just $ VBool (a < b)
    (VInt a, VFloat b)   -> Just $ VBool (fromIntegral a < b)
    (VFloat a, VInt b)   -> Just $ VBool (a < fromIntegral b)
    _ -> Nothing 

--Greater Than
eval vars (Gt x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VBool (a > b)
    (VFloat a, VFloat b) -> Just $ VBool (a > b)
    (VInt a, VFloat b)   -> Just $ VBool (fromIntegral a > b)
    (VFloat a, VInt b)   -> Just $ VBool (a > fromIntegral b)
    _ -> Nothing

--Less than or equal (<=)
eval vars (Leq x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VBool (a <= b)
    (VFloat a, VFloat b) -> Just $ VBool (a <= b)
    (VInt a, VFloat b)   -> Just $ VBool (fromIntegral a <= b)
    (VFloat a, VInt b)   -> Just $ VBool (a <= fromIntegral b)
    _ -> Nothing

--Greater than or equal (>=)
eval vars (Geq x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Just $ VBool (a >= b)
    (VFloat a, VFloat b) -> Just $ VBool (a >= b)
    (VInt a, VFloat b)   -> Just $ VBool (fromIntegral a >= b)
    (VFloat a, VInt b)   -> Just $ VBool (a >= fromIntegral b)
    _ -> Nothing


--helper function to convert char to int
digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

-- function for converting multiple digits to integers
stringToInt :: String -> Int 
stringToInt ns = foldl (\a x -> a * 10 + digitToInt x) 0 ns

-- added token to ignore leading and trailing spaces
pCommand :: Parser Command
pCommand = do 
              cmd <- command -- parsing main command 
              Parsing.many comment
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
           string ":loop "
           ns <- many1 digit 
           cmd <- command
           return (Loop (read ns) cmd))

    comment = token (do 
         char '#'
         comment <- Parsing.many (sat (\x -> x /= '\n'))
         return (Comment comment))

    emptyLine = token (do 
                Parsing.many (sat (\x -> x `elem` " \t"))
                return EmptyLine)



--expression parser
--for add and sub
-- new parsers for boolean and comparison operators
pExpr :: Parser Expr
pExpr = pOr

pOr :: Parser Expr
pOr = do
  e1 <- pAnd
  (do symbol "||"
      e2 <- pOr
      return (Or e1 e2)
   ) <|> return e1

pAnd :: Parser Expr
pAnd = do
  e1 <- pComp
  (do symbol "&&"
      e2 <- pAnd
      return (And e1 e2)
   ) <|> return e1

pComp :: Parser Expr
pComp = do
  e1 <- pAddSub
  (do op <- choice [symbol "==", symbol "!=", symbol "<", symbol ">", symbol "<=", symbol ">="]
      e2 <- pAddSub
      case op of
        "==" -> return (Eq e1 e2)
        "!=" -> return (Neq e1 e2)
        "<"  -> return (Lt e1 e2)
        ">"  -> return (Gt e1 e2)
        "<=" -> return (Leq e1 e2)
        ">=" -> return (Geq e1 e2)
   ) <|> return e1

pAddSub :: Parser Expr
pAddSub = do
  t <- pTerm
  rest t

rest :: Expr -> Parser Expr
rest t = (do symbol "+"
             e <- pTerm
             rest (Add t e))
         <|> (do symbol "-"
                 e <- pTerm
                 rest (Sub t e))
         <|> return t


pTerm :: Parser Expr
pTerm = do
  symbol "not"
  e <- pFactor
  return (Not e)
  <|> do symbol "abs"
         symbol "("
         e <- pExpr
         symbol ")"
         return (Abs e)
  <|> do symbol "sin"
         symbol "("
         e <- pExpr
         symbol ")"
         return (Sin e)
  <|> do symbol "cos"
         symbol "("
         e <- pExpr
         symbol ")"
         return (Cos e)
  <|> do symbol "tan"
         symbol "("
         e <- pExpr
         symbol ")"
         return (Tan e)
  <|> do f <- pFactor
         (do symbol "*"
             t <- pTerm
             return (Mul f t)
          <|> do symbol "/"
                 t <- pTerm
                 return (Div f t)
          <|> do symbol "^"
                 t <- pTerm
                 return (Power f t)
          <|> do symbol "%"
                 t <- pTerm
                 return (Mod f t)
          <|> return f)


pFactor :: Parser Expr
pFactor = do d <- double
             return (Val (VFloat d))
        <|> do n <- integer
               return (Val (VInt n))
        <|> do symbol "true"
               return (Val (VBool True))
        <|> do symbol "false"
               return (Val (VBool False))
        <|> do v <- identifier
               return (Var v)
        <|> do symbol "("
               e <- pExpr
               symbol ")"
               return e
