module Interpreter where

import Data.Char
import Data.List
import Data.Maybe
import Expressions
import Token
import Ast
import Inst
import Parser
import Translator

readString :: String -> [Inst]
readString str = read str :: [Inst]

saveBytecode :: [Inst] -> String -> IO ()
saveBytecode bc path
  = do
    writeFile path (show bc)

openBytecode :: String -> IO [Inst]
openBytecode path
  = do
    s <- readFile path
    return (readString s)

data Eval
  = Number Rational
  | Character Char
  | EList [Eval]
  | Nil
  deriving(Show)

-- TODO
int instructions
  = let
      -- Load all variables
      varStack :: [(String, Inst)]
      varStack
        = let
            search [] = []
            search ((Function (PushVar name) args body) : rest)
              = (name, Lambda args body) : search rest

            search ((AssignTo (DeclVar var) value) : rest)
              = (var, value) : search rest

            search (_:rest)
              = search rest
          in
            search instructions

      
      varTypes = map (\a b _ -> (a, b)) varStack

      --
      --eval :: Fractional a => Inst -> a
      eval e
        = case e of
            Operation "+" a b -> (eval a) + (eval b)
            Operation "-" a b -> (eval a) - (eval b)
            Operation "*" a b -> (eval a) * (eval b)
            Operation "/" a b -> (eval a) / (eval b)

            PushVar x -> let (Just a) = lookup x varStack in eval a
            PushConst x ->  read x

            _ -> 0

    in
      map eval instructions

