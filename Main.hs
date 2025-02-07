module Main where

import Parsing
import Expr
import REPL

main :: IO ()
main = repl initREPLState

