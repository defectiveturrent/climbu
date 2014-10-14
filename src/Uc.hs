module Main where

import System.IO
import System.Environment
import System.Process
import Data.Char
import Data.Maybe
import Data.List
import Expressions
import Parser
import Translator

-- Get input arguments
main
  = do
      (exec:rest) <- getArgs
      code <- readFile $ head rest
      let
        gen = genCode code

      writeFile ((head rest) ++ ".cpp") gen
      system $ "g++ -std=c++14 -o" ++ exec ++ " " ++ ((head rest) ++ ".cpp")
