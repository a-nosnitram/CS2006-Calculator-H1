module Expr where

import Parsing
    ( char,
      digit,
      identifier,
      integer,
      many,
      many1,
      sat,
      string,
      symbol,
      token,
      (|||),
      Parser,
      double,
      choice )
import Data.Fixed (mod')
import Control.Applicative hiding (many)
import Data.List (groupBy, sortOn)
import Parsing (space)

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
             | Simplify Expr --- Simplify expressions 
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

       ||| token (do
            symbol ":simplify"
            e <- pExpr
            return (Simplify e))

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

-- This is the factor parser with 
-- added multi-digit support
-- and negative number support
pFactor = do
    -- Parse negative variables
    symbol "-"
    v <- identifier
    return (Mul (Val (VInt (-1))) (Var v))
  <|> do
    num <- integer
    var <- identifier
    return (Mul (Val (VInt num)) (Var var))
  <|> do d <- double
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


-------------------------------------------------------------------
instance Show Expr where
  show (Add e1 e2) = 
    case e2 of
      -- If the second operand is negative, rewrite as subtraction
      Val (VInt n) | n < 0 -> show e1 ++ " - " ++ show (Val (VInt (-n)))
      Val (VFloat n) | n < 0 -> show e1 ++ " - " ++ show (Val (VFloat (-n)))
      _ -> show e1 ++ " + " ++ show e2  -- Otherwise, standard addition

  -- Show a subtraction expression
  show (Sub e1 e2) = show e1 ++ " - " ++ show e2

  -- Show a multiplication expression with added parentheses for sub-expressions
  show (Mul e1 e2) = showFactor e1 ++ " * " ++ showFactor e2

   -- Helper function to add parentheses around additions and subtractions
    where showFactor (Add a b) = "(" ++ show (Add a b) ++ ")"
          showFactor (Sub a b) = "(" ++ show (Sub a b) ++ ")"
          showFactor e = show e
  show (Div e1 e2) = show e1 ++ " / " ++ show e2
  show (Var x) = x
  show (Val v) = show v
  show (Power e1 e2) = show e1 ++ " ^ " ++ show e2
  show (Mod e1 e2) = show e1 ++ " % " ++ show e2
  show (Abs e) = "abs(" ++ show e ++ ")"
  show (Sin e) = "sin(" ++ show e ++ ")"
  show (Cos e) = "cos(" ++ show e ++ ")"
  show (Tan e) = "tan(" ++ show e ++ ")"
  show (And e1 e2) = show e1 ++ " && " ++ show e2
  show (Or e1 e2) = show e1 ++ " || " ++ show e2
  show (Not e) = "not(" ++ show e ++ ")"
  show (Eq e1 e2) = show e1 ++ " == " ++ show e2
  show (Neq e1 e2) = show e1 ++ " != " ++ show e2
  show (Lt e1 e2) = show e1 ++ " < " ++ show e2
  show (Gt e1 e2) = show e1 ++ " > " ++ show e2
  show (Leq e1 e2) = show e1 ++ " <= " ++ show e2
  show (Geq e1 e2) = show e1 ++ " >= " ++ show e2

-- Custom Eq instance for the Expr data type
instance Eq Expr where
  (Val v1) == (Val v2) = v1 == v2
  (Var x) == (Var y) = x == y
  (Add e1 e2) == (Add f1 f2) = e1 == f1 && e2 == f2
  (Sub e1 e2) == (Sub f1 f2) = e1 == f1 && e2 == f2
  (Mul e1 e2) == (Mul f1 f2) = e1 == f1 && e2 == f2
  (Div e1 e2) == (Div f1 f2) = e1 == f1 && e2 == f2
  (Power e1 e2) == (Power f1 f2) = e1 == f1 && e2 == f2
  (Mod e1 e2) == (Mod f1 f2) = e1 == f1 && e2 == f2
  (Abs e1) == (Abs e2) = e1 == e2
  (Sin e1) == (Sin e2) = e1 == e2
  (Cos e1) == (Cos e2) = e1 == e2
  (Tan e1) == (Tan e2) = e1 == e2
  (And e1 e2) == (And f1 f2) = e1 == f1 && e2 == f2
  (Or e1 e2) == (Or f1 f2) = e1 == f1 && e2 == f2
  (Not e1) == (Not e2) = e1 == e2
  (Eq e1 e2) == (Eq f1 f2) = e1 == f1 && e2 == f2
  (Neq e1 e2) == (Neq f1 f2) = e1 == f1 && e2 == f2
  (Lt e1 e2) == (Lt f1 f2) = e1 == f1 && e2 == f2
  (Gt e1 e2) == (Gt f1 f2) = e1 == f1 && e2 == f2
  (Leq e1 e2) == (Leq f1 f2) = e1 == f1 && e2 == f2
  (Geq e1 e2) == (Geq f1 f2) = e1 == f1 && e2 == f2
  _ == _ = False  -- If expressions don't match, return False


-- Helper function to collect terms in an addition
collectTerms :: Expr -> [Expr]
collectTerms (Add e1 e2) = collectTerms e1 ++ collectTerms e2
collectTerms e = [e] -- Base case: a single term

-- Helper function to extract the variable name from expressions
varNameKey :: Expr -> String
varNameKey (Var x) = x 
varNameKey (Mul (Val _) (Var x)) = x
varNameKey e = show e  -- Fallback for non-variable terms

-- Helper function to group like terms together
groupLikeTerms :: [Expr] -> [Expr]
groupLikeTerms terms =
  let
    -- Separate numerical values and variables
    (numbers, variables) = foldr (\e (nums, vars) -> case e of
                                    Val v -> (v : nums, vars)
                                    _     -> (nums, e : vars)
                        ) ([], []) terms

    -- Sum all numbers together
    sumNumbers = case numbers of
      [] -> Nothing
      vals -> Just (Val (foldr addValues (VInt 0) vals))

    -- Sort variables by their name key and group
    sortedVars = sortOn varNameKey variables
    groupedVars = map sumVariableGroup $ groupBy sameVarName sortedVars
    
  in
    -- If the sum of numbers is 0, ignore it. Otherwise, combine the results
    case sumNumbers of
      Just (Val (VInt 0)) -> groupedVars  -- If sum is 0, ignore it
      Just v -> v : groupedVars
      Nothing -> groupedVars

-- Function to sum a group of like terms
sumVariableGroup :: [Expr] -> Expr
sumVariableGroup grp =
  let
    -- Extract coefficients from terms like 3*x
    extractCoeff :: Expr -> (Int, String)
    extractCoeff (Mul (Val (VInt c)) (Var x)) = (c, x)  -- Extract c*x
    extractCoeff (Var x) = (1, x)  -- Treat x as 1*x
    extractCoeff expr = (1, show expr)

    -- Get all coefficients for the same variable
    coeffPairs = map extractCoeff grp
    (coeffs, vars) = unzip coeffPairs
    totalCoeff = sum coeffs
  in
    if totalCoeff == 1 then Var (head vars)  -- If only x, keep as x
    else Mul (Val (VInt totalCoeff)) (Var (head vars))  -- Otherwise, c*x

-- Helper function to check if two variables are the same
sameVarName :: Expr -> Expr -> Bool
sameVarName (Var x) (Var y) = x == y
sameVarName (Mul (Val (VInt _)) (Var x)) (Var y) = x == y
sameVarName (Var x) (Mul (Val (VInt _)) (Var y)) = x == y
sameVarName (Mul (Val (VInt _)) (Var x)) (Mul (Val (VInt _)) (Var y)) = x == y
sameVarName _ _ = False

-- Simplification function
simplifyExpr :: Expr -> Expr
-- Addition Simplifications
simplifyExpr (Add e1 e2) =
  let 
      e1' = simplifyExpr e1  -- First simplify left side
      e2' = simplifyExpr e2  -- Then simplify right side
      simplifiedTerms = collectTerms (Add e1' e2') -- Collect all terms
      groupedTerms = groupLikeTerms simplifiedTerms -- Group like terms
  in case groupedTerms of
       [Mul (Val (VInt (-1))) (Var x), Mul (Val (VInt c)) (Var y)]
          | x == y -> Mul (Val (VInt (c - 1))) (Var x)  -- Example: -x + 3*x → 2*x
       [single] -> single  -- If there's only one term, return it directly
       terms -> foldr1 Add terms  -- Otherwise, reconstruct the expression

-- Subtraction Simplifications
simplifyExpr (Sub e1 e2) =
  let e1' = simplifyExpr e1
      e2' = simplifyExpr e2
  in case (e1', e2') of

    -- Numeric simplifications
    (Val (VInt a), Val (VInt b)) -> Val (VInt (a - b))
    (Val (VFloat a), Val (VFloat b)) -> Val (VFloat (a - b))
    (Val (VInt a), Val (VFloat b)) -> Val (VFloat (fromIntegral a - b))
    (Val (VFloat a), Val (VInt b)) -> Val (VFloat (a - fromIntegral b))

    -- Edge cases for zero
    (e', Val (VInt 0)) -> e'  -- x - 0 → x
    (Val (VInt 0), e') -> Mul (Val (VInt (-1))) e'  -- 0 - x → -x
    (e1', e2') | e1' == e2' -> Val (VInt 0)  -- x - x → 0

    -- Subtracting variable terms with coefficients (e.g., 5x - 3x → 2x)
    (Mul (Val (VInt c1)) (Var x), Mul (Val (VInt c2)) (Var y)) | x == y ->
      let newCoeff = c1 - c2
      in if newCoeff == 0 then Val (VInt 0)
         else Mul (Val (VInt newCoeff)) (Var x)  -- Properly combine coefficients

    -- Subtracting a constant from a term (e.g., x - 3 should be x - 3 not -1 * 3 + x)
    (Var x, Val (VInt c)) -> Add (Var x) (Val (VInt (-c)))
    (Val (VInt c), Var x) -> Add (Val (VInt c)) (Mul (Val (VInt (-1))) (Var x))

    -- Convert a - b into a + (-b) and simplify
    _ ->
      let simplified = Add e1' (Mul (Val (VInt (-1))) e2')  -- Fixed parenthesis
          simplified' = simplifyExpr simplified 
      in simplified'  -- Use the simplified expression

-- Multiplication Simplifications
simplifyExpr (Mul e1 e2) =
  let e1' = simplifyExpr e1
      e2' = simplifyExpr e2
  in case (e1', e2') of
    -- Numeric multiplication
    (Val (VInt a), Val (VInt b)) -> Val (VInt (a * b))
    (Val (VFloat a), Val (VFloat b)) -> Val (VFloat (a * b))
    (Val (VInt a), Val (VFloat b)) -> Val (VFloat (fromIntegral a * b))
    (Val (VFloat a), Val (VInt b)) -> Val (VFloat (a * fromIntegral b))

    -- Identity cases
    (Val (VInt 1), e') -> e'
    (e', Val (VInt 1)) -> e'
    (Val (VInt 0), _) -> Val (VInt 0)
    (_, Val (VInt 0)) -> Val (VInt 0)

    -- Multiplication of like variables (`x * x` → `x^2`)
    (Var x, Var y) | x == y -> Power (Var x) (Val (VInt 2))

    -- Multiplication of `x^a * x` → `x^(a+1)`
    (Power (Var x1) (Val (VInt a)), Var x2) | x1 == x2 ->
        Power (Var x1) (Val (VInt (a + 1)))

    -- Multiplication of `x * x^b` → `x^(b+1)`
    (Var x1, Power (Var x2) (Val (VInt b))) | x1 == x2 ->
        Power (Var x1) (Val (VInt (b + 1)))

    -- Multiplication of `x^a * x^b` → `x^(a+b)`
    (Power (Var x1) (Val (VInt a)), Power (Var x2) (Val (VInt b))) | x1 == x2 ->
        Power (Var x1) (Val (VInt (a + b)))

    -- Default case: return the expression unchanged
    _ -> Mul e1' e2'




-- Division Simplifications
simplifyExpr (Div e1 e2) =
  let e1' = simplifyExpr e1
      e2' = simplifyExpr e2
  in case (e1', e2') of
    -- Numeric division
    (Val (VInt a), Val (VInt b)) | b /= 0 && a `mod` b == 0 -> Val (VInt (a `div` b))
    (Val (VFloat a), Val (VFloat b)) | b /= 0 -> Val (VFloat (a / b))

    -- Edge case: x / 1 → x
    (e, Val (VInt 1)) -> e
    (e, Val (VFloat 1.0)) -> e

    -- x / x → 1
    (Var x, Var y) | x == y -> Val (VInt 1)

    -- (c * x) / c → x  (e.g., (2x) / 2 → x)
    (Mul (Val (VInt c)) v, Val (VInt d)) | d /= 0 && c `mod` d == 0 ->
        if c `div` d == 1 then v else Mul (Val (VInt (c `div` d))) v

    -- (ax + b) / a → x + (b / a) (e.g., (2x + 4) / 2 → x + 2)
    (Add (Mul (Val (VInt a)) x) (Val (VInt b)), Val (VInt c))
      | a == c && c /= 0 && b `mod` c == 0 -> Add x (Val (VInt (b `div` c)))
      | a == c && c /= 0 -> Add x (Div (Val (VInt b)) (Val (VInt c)))

    --Factor out c from ax + bx expressions correctly
    (Add (Mul (Val (VInt c1)) x) (Mul (Val (VInt c2)) y), Val (VInt c))
      | c1 `mod` c == 0 && c2 `mod` c == 0 -> Add (Mul (Val (VInt (c1 `div` c))) x)
                                                 (Mul (Val (VInt (c2 `div` c))) y)

    -- Distribute division over addition (e.g., (6x + 3) / 3 → 2x + 1)
    (Add t1 t2, Val (VInt d)) | d /= 0 ->
      let t1' = simplifyExpr (Div t1 (Val (VInt d)))
          t2' = simplifyExpr (Div t2 (Val (VInt d)))
      in Add t1' t2'

    -- Power simplification: x^a / x^b → x^(a-b)
    (Power x1 n1, Power x2 n2) | x1 == x2 -> Power x1 (Sub n1 n2)

    -- Distribute division over addition and simplify the result
    (Add t1 t2, Val (VInt d)) | d /= 0 ->
      let t1' = simplifyExpr (Div t1 (Val (VInt d)))
          t2' = simplifyExpr (Div t2 (Val (VInt d)))
          simplifiedAdd = simplifyExpr (Add t1' t2')  --recursive simplification
      in simplifiedAdd
      
    -- Default case: no further simplification
    _ -> Div e1' e2'

-- Default case: return the expression unchanged if no rule applies
simplifyExpr e = e

addValues :: Value -> Value -> Value
addValues (VInt a) (VInt b) = VInt (a + b)
addValues (VFloat a) (VFloat b) = VFloat (a + b)
addValues (VInt a) (VFloat b) = VFloat (fromIntegral a + b)
addValues (VFloat a) (VInt b) = VFloat (a + fromIntegral b)

-- Parses strings 
pStrVal :: Parser Expr 
pStrVal = do 
   char '"'
   s <- many (sat (\x -> x /= '"'))
   char '"'
   return (StrVal s) 

