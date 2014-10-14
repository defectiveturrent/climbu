module Main where

import System.IO
import System.Environment
import System.Process
import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Expressions
import Parser
import Translator

main
  = do
      forever interpret

interpret
  = do
      line <- getLine
      writeFile ".uci.cpp" (genCode line)
      system "g++ -std=c++14 -o.ucic .uci.cpp"
      system "rm .uci.cpp"
      system "./.ucic"
      system "rm .ucic"
      putStrLn []
