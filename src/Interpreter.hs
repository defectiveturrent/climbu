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

-- TODO
int instructions
  = let
      varsStack = 