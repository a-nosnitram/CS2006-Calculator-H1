module Expr where

import Parsing
import Data.Fixed (mod')
import Control.Applicative hiding (many)

type Name = String

-- Data type for representing values
data Value = VInt Int | VFloat Double | VBool Bool | VString String deriving (Eq)

-- Custom display logic for Value instance
instance Show Value where
  show (VInt x) = show x
  show (VFloat x) 
    | x == fromIntegral (floor x) = show (floor x) -- shows as int for whole numbers
    | otherwise = show (fromIntegral (round (x * 1000000)) / 1000000) -- rounded float to avoid precision errors
  show (VBool b) = show b
  show (VString s) = show s

-- Expression data type representing arithmetic, boolean, and comparison operations
data Expr = Add Expr Expr   -- Addition
          | Sub Expr Expr   -- Subtraction
          | Mul Expr Expr   -- Multiplication
          | Div Expr Expr   -- Division
          | Var Name        -- Variable reference
          | StrVal String   -- String literal
          | Concat Expr Expr -- String concatenation
          | Val Value       -- Literal values (integers, floats, booleans, strings)
          | Power Expr Expr -- Exponentiation
          | Mod Expr Expr   -- Modulo operation
          | Abs Expr        -- Absolute value
          | Sin Expr        -- Sine function
          | Cos Expr        -- Cosine function
          | Tan Expr        -- Tangent function
          | And Expr Expr   -- Boolean AND
          | Or Expr Expr    -- Boolean OR
          | Not Expr        -- Boolean NOT
          | Eq Expr Expr    -- Equality (==)
          | Neq Expr Expr   -- Inequality (!=)
          | Lt Expr Expr    -- Less than (<)
          | Gt Expr Expr    -- Greater than (>)
          | Leq Expr Expr   -- Less than or equal (<=)
          | Geq Expr Expr   -- Greater than or equal (>=)
          deriving (Show, Eq)


-- Commands for a REPL (Read-Eval-Print Loop)
data Command = Set Name Expr  -- Assign variable
             | Eval Expr      -- Evaluate an expression
             | Quit          -- Exit the REPL
             | History Int   -- Get command history
             | Clear         -- Clear history
             | Comment String -- Comments in input
             | EmptyLine     -- Ignore empty lines
             | Loop Int [Command] -- Loop command for repeated execution
             | Print [Expr]  -- Print command for displaying expressions
             deriving (Show, Eq)

-- Binary search tree for storing variable values
data VarTree = Empty
             | Node Name Value VarTree VarTree
  deriving (Show, Eq)

-- Function to lookup a variable's value in the binary search tree
lookupVar :: Name -> VarTree -> Maybe Value
lookupVar _ Empty = Nothing
lookupVar name (Node n v left right)
  | name == n  = Just v
  | name < n   = lookupVar name left
  | otherwise  = lookupVar name right

-- Evaluator function for expressions
eval :: VarTree -> Expr -> Either String Value

-- Handle numeric values directly
eval _ (Val v) = Right v

-- Handle strings 
eval vars (StrVal s) = Right (VString s)

-- Handle variables (look up their value in the variable list)
eval vars (Var x) = case lookupVar x vars of
    Just (VString s) -> Right (VString s)
    Just v  -> Right v
    Nothing -> Left $ "Variable not found: " ++ x


--Addition--------------------
eval vars (Add x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Right $ VInt (a + b)
    (VFloat a, VFloat b) -> Right $ VFloat (a + b)
    (VInt a, VFloat b)   -> Right $ VFloat (fromIntegral a + b)
    (VFloat a, VInt b)   -> Right $ VFloat (a + fromIntegral b)
    _ -> Left "Type mismatch in addition: operands must be numeric"

--Subtraction--------------------
eval vars (Sub x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Right $ VInt (a - b)
    (VFloat a, VFloat b) -> Right $ VFloat (a - b)
    (VInt a, VFloat b)   -> Right $ VFloat (fromIntegral a - b)
    (VFloat a, VInt b)   -> Right $ VFloat (a - fromIntegral b)
    _ -> Left "Type mismatch in subtraction: operands must be numeric"

--Multiplication-------------------
eval vars (Mul x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Right $ VInt (a * b)
    (VFloat a, VFloat b) -> Right $ VFloat (a * b)
    (VInt a, VFloat b)   -> Right $ VFloat (fromIntegral a * b)
    (VFloat a, VInt b)   -> Right $ VFloat (a * fromIntegral b)
    _ -> Left "Type mismatch in multiplication: operands must be numeric"

--Division-------------------
eval vars (Div x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case y' of
    VInt 0     -> Left "Division by zero"
    VFloat 0.0 -> Left "Division by zero"
    _ -> case (x', y') of
      (VInt a, VInt b)     -> Right $ VFloat (fromIntegral a / fromIntegral b)
      (VFloat a, VFloat b) -> Right $ VFloat (a / b)
      (VInt a, VFloat b) -> Right $ VFloat (fromIntegral a / b)
      (VFloat a, VInt b) -> Right $ VFloat (a / fromIntegral b)
      _ -> Left "Type mismatch in division: operands must be numeric"

--Power (exponentiation)-----------------
eval vars (Power x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Right $ VInt (a ^ b)
    (VFloat a, VFloat b) -> Right $ VFloat (a ** b)
    (VInt a, VFloat b)   -> Right $ VFloat (fromIntegral a ** b)
    (VFloat a, VInt b)   -> Right $ VFloat (a ** fromIntegral b)
    _-> Left "Error: Type mismatch in exponentiation"

--Modulo-------------------------
eval vars (Mod x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case y' of
    VInt 0     -> Left "Modulo by zero"
    VFloat 0.0 -> Left "Modulo by zero"
    _ -> case (x', y') of
      (VInt a, VInt b)   -> Right $ VInt (a `mod` b)
      (VFloat a, VFloat b) -> Right $ VFloat (a `mod'` b)
      (VInt a, VFloat b) -> Right $ VFloat (fromIntegral a `mod'` b)
      (VFloat a, VInt b) -> Right $ VFloat (a `mod'` fromIntegral b)
      _ -> Left "Type mismatch in modulo operation"

--Absolute Value------------------------
eval vars (Abs x) = do
  x' <- eval vars x
  case x' of
    VInt a   -> Right $ VInt (abs a)
    VFloat a -> Right $ VFloat (abs a)

--String Concatenation-------------------
eval vars (Concat x y) = do 
   x' <- eval vars x
   y' <- eval vars y
   case (x', y') of 
     (VString a, VString b) -> Right $ VString (a ++ b)
     (VString a, VInt b) -> Right $ VString (a ++ show b)
     (VInt a, VString b) -> Right $ VString (show a ++ b)
     (VFloat a, VString b) -> Right $ VString (show a ++ b)
     (VString a, VFloat b) -> Right $ VString (a ++ show b)
     (VBool a, VString b) -> Right $ VString (show a ++ b)
     (VString a, VBool b) -> Right $ VString (a ++ show b)
     _ -> Left "Cannot concatenate non-string values"


--Trigonometric functions----------------
--sine
eval vars (Sin x) = do
  x' <- eval vars x
  case x' of
        VInt a   -> Right $ VFloat (sin (fromIntegral a))  
        VFloat a -> Right $ VFloat (sin a)  
        _        -> Left "Type mismatch: sin expects a number"

--cosine
eval vars (Cos x) = do
  x' <- eval vars x
  case x' of
        VInt a   -> Right $ VFloat (cos (fromIntegral a))  
        VFloat a -> Right $ VFloat (cos a)  
        _        -> Left "Type mismatch: cos expects a number"

--tangent
eval vars (Tan x) = do
  x' <- eval vars x
  case x' of
        VInt a   -> let angle = fromIntegral a
                    in if abs (cos angle) < 1e-10
                       then Left "Undefined value for tan(x) at this angle"
                       else Right $ VFloat (tan angle)
        VFloat a -> if abs (cos a) < 1e-10
                    then Left "Undefined value for tan(x) at this angle"
                    else Right $ VFloat (tan a)
        _ -> Left "Type mismatch: tan expects a number"

--boolean operators-------------------------------
--And (&&)
eval vars (And x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VBool a, VBool b) -> Right $ VBool (a && b)
    _ -> Left "Both operands must be booleans for AND operation"

eval vars (Or x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VBool a, VBool b) -> Right $ VBool (a || b)
    _ -> Left "Both operands must be booleans for OR operation"

--Not
eval vars (Not x) = do
  x' <- eval vars x
  case x' of
    VBool a -> Right $ VBool (not a)
    _ -> Left "Operand must be a boolean for NOT operation"

--Comparason operators--------
--Equality (==)
eval vars (Eq x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Right $ VBool (a == b)
    (VFloat a, VFloat b) -> Right $ VBool (a == b)
    (VInt a, VFloat b)   -> Right $ VBool (fromIntegral a == b)
    (VFloat a, VInt b)   -> Right $ VBool (a == fromIntegral b)
    (VBool a, VBool b)   -> Right $ VBool (a == b)
    _ -> Left "Cannot compare values of different types"

--inEquality (/=)
eval vars (Neq x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Right $ VBool (a /= b)
    (VFloat a, VFloat b) -> Right $ VBool (a /= b)
    (VInt a, VFloat b)   -> Right $ VBool (fromIntegral a /= b)
    (VFloat a, VInt b)   -> Right $ VBool (a /= fromIntegral b)
    (VBool a, VBool b)   -> Right $ VBool (a /= b)
    _ -> Left "Cannot compare values of different types" 

--Less Than (<)
eval vars (Lt x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Right $ VBool (a < b)
    (VFloat a, VFloat b) -> Right $ VBool (a < b)
    (VInt a, VFloat b)   -> Right $ VBool (fromIntegral a < b)
    (VFloat a, VInt b)   -> Right $ VBool (a < fromIntegral b)
    _ -> Left "Cannot compare values of different types"

--Greater Than (>)
eval vars (Gt x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Right $ VBool (a > b)
    (VFloat a, VFloat b) -> Right $ VBool (a > b)
    (VInt a, VFloat b)   -> Right $ VBool (fromIntegral a > b)
    (VFloat a, VInt b)   -> Right $ VBool (a > fromIntegral b)
    _ -> Left "Cannot compare values of different types"

--Less than or equal (<=)
eval vars (Leq x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Right $ VBool (a <= b)
    (VFloat a, VFloat b) -> Right $ VBool (a <= b)
    (VInt a, VFloat b)   -> Right $ VBool (fromIntegral a <= b)
    (VFloat a, VInt b)   -> Right $ VBool (a <= fromIntegral b)
    _ -> Left "Cannot compare values of different types"

--Greater than or equal (>=)
eval vars (Geq x y) = do
  x' <- eval vars x
  y' <- eval vars y
  case (x', y') of
    (VInt a, VInt b)     -> Right $ VBool (a >= b)
    (VFloat a, VFloat b) -> Right $ VBool (a >= b)
    (VInt a, VFloat b)   -> Right $ VBool (fromIntegral a >= b)
    (VFloat a, VInt b)   -> Right $ VBool (a >= fromIntegral b)
    _ -> Left "Cannot compare values of different types"

-- Helper function to convert char to int
digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

-- Parser for commands
pCommand :: Parser Command
pCommand = do
              space
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
            string ":q" 
            return Quit) 
       ||| token (do 
            string ":c"
            return Clear)
       ||| token (do 
            char ':'   
            ns <- many1 digit 
            return (History (read ns)))
       ||| token (do 
            string ":print"
            space
            expr <- many1 pExpr 
            return (Print expr))
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
         comment <- Parsing.many (sat (\x -> x /= '\n'))
         return (Comment comment))

    emptyLine = token (do 
                Parsing.many (sat (\x -> x `elem` " \t"))
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

-- Parsers for boolean and comparison operators
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
  (do op <- choice [symbol "<=", symbol ">=", symbol "==", symbol "!=", symbol "<", symbol ">"]
      e2 <- pAddSub
      case op of
        "<=" -> return (Leq e1 e2)
        ">=" -> return (Geq e1 e2)
        "==" -> return (Eq e1 e2)
        "!=" -> return (Neq e1 e2)
        "<"  -> return (Lt e1 e2)
        ">"  -> return (Gt e1 e2)
   ) <|> return e1

pAddSub :: Parser Expr
pAddSub = do
  t <- pTerm
  rest t

rest :: Expr -> Parser Expr
rest t = (do symbol "++"
             e <- pExpr
             return (Concat t e))
         <|> (do symbol "+"
                 e <- pTerm
                 rest (Add t e))
         <|> (do symbol "-"
                 e <- pTerm
                 rest (Sub t e))
         <|> return t

pTerm :: Parser Expr
pTerm = pStrVal
  <|> do symbol "not"
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

-- Parses strings 
pStrVal :: Parser Expr 
pStrVal = do 
   char '"'
   s <- many (sat (\x -> x /= '"'))
   char '"'
   return (StrVal s) 

