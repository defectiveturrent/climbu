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
      (string:_) <- getArgs
      let
        outcode = intercalate "\n" . execute . tokenise $ ("def main=println with " ++ string)
        finalcode = "#include \"standard.hpp\"\n\n" ++ outcode

      writeFile ".uci.cpp" finalcode
      system "g++ -std=c++14 -o.ucic .uci.cpp"
      system "rm .uci.cpp"
      system "./.ucic"
      system "rm .ucic"
