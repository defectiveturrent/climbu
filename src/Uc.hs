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
        outcode = intercalate "\n" . execute . tokenise $ code
        finalcode = "#include \"standard.hpp\"\n\n" ++ outcode

      writeFile ((head rest) ++ ".cpp") finalcode
      system $ "g++ -std=c++14 -o" ++ exec ++ " " ++ ((head rest) ++ ".cpp")
