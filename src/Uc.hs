{-
    Climbu compiler / interpreter
    Copyright (C) 2014  Mario Feroldi

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

module Main where

import System.Environment
import System.IO
import System.IO.Error
import System.Process
import Data.Char
import Data.Maybe
import Data.List
import Expressions
import Parser
import Translator

main = toTry `catch` handler

catch = catchIOError

toTry :: IO ()
toTry = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch in action args -- gets the function that corresponds to argument

handler :: IOError -> IO ()  
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e  

commands :: [String]
commands = [ "-c"        -- compile
           , "-x"        -- interpret
           , "-v"        -- version
           , "--version"
           , "-h"        -- help
           , "--help"
           ]

dispatch :: [ (String, [String] -> IO ()) ]
dispatch = zip commands [ compile
                        , interpret
                        , version
                        , version
                        , help
                        , help
                        ]

compile (executable:pathsource:_)
  = do
      stringsource <- readFile pathsource
      generatedCode <- return $ genCode stringsource
      filebyteName <- return $ executable ++ ".cpp"
      writeFile filebyteName generatedCode
      system $ "g++ -std=c++14 -o" ++ executable ++ " " ++ filebyteName
      return ()

interpret (lines:_)
  = let
      generatedCode = genCode $ "def main = println . " ++ lines

    in do
        writeFile ".Climbu.cpp" generatedCode
        system "g++ -std=c++14 -o.Climbu .Climbu.cpp"
        system "./.Climbu"
        system "rm -rf .Climbu"
        system "rm -rf .Climbu.cpp"
        return ()

version _
  = do
      putStrLn "Climbu compiler v1.1 - Copyright (C) 2014  Mario Feroldi"
      putStrLn "This program comes with ABSOLUTELY NO WARRANTY."
      putStrLn "This is free software, and you are welcome to redistribute it"
      putStrLn "under GPL v3 license.\n"

help _
  = do
      putStrLn "Usage: climbu [option] file...\n"
      putStrLn "  -c <bin name> <file>    Compiles a Climbu code"
      putStrLn "  -x <line>                      Interprets a Climbu code"
      putStrLn "  {-v --version}                 Show version"
      putStrLn "  {-h --help}                    Show help"
      putStrLn []
      putStrLn "Report bugs to <blueoatstudio@gmail.com>"
