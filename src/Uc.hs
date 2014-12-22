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
import qualified Translator as Cpp
-- import qualified Lua

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
commands = [ "-c"             -- compile
           , "-i"             -- interpret
           , "-v"             -- version
           , "--version"      -- version
           , "-h"             -- help
           , "--help"         -- help
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
      generatedCode <- return $ Cpp.genCode stringsource
      case generatedCode of
        Right clines ->
          do
            filebyteName <- return $ executable ++ ".cpp"
            writeFile filebyteName clines
            system $ "clang++ -std=c++14 -Wc++11-extensions -o" ++ executable ++ " " ++ filebyteName
            return ()

        Left msg ->
          putStrLn msg
      return ()

interpret (lines:_)
  = let
      generatedCode = Cpp.genCode $ "main() = println (" ++ lines ++ ");"

    in do
        case generatedCode of
          Right clines ->
            do
              writeFile ".cpp_temp_cl.cpp" clines
              system "clang++ -std=c++14 -Wc++11-extensions .cpp_temp_cl.cpp -o .tmpcl"
              system "./.tmpcl"
              return ()

          Left msg ->
            putStrLn msg
        return ()

version _
  = do
      putStrLn "Climbu compiler v1.6.2 (Painting with types) - Copyright (C) 2014  Mario Feroldi"
      putStrLn "This program comes with ABSOLUTELY NO WARRANTY."
      putStrLn "This is free software, and you are welcome to redistribute it"
      putStrLn "under GPL v3 license.\n"

help ("-c":_)
  = do
      putStrLn "COMPILE <binary name> <source/file>"
      putStrLn "\n   This command compiles a source or file to an executable binary file."
      putStrLn "\n     For example:"
      putStrLn "        climbu -c test source.cm"
      putStrLn "\n To see more, type --help."

help _
  = do
      putStrLn "Usage: climbu [option] file...\n"
      putStrLn "  -c <bin name> <file>           Compiles a Climbu code"
      putStrLn "  -i <line>                      Interprets a line"
      putStrLn "  {-v --version}                 Shows version"
      putStrLn "  {-h --help}                    Shows help"
      putStrLn []
      putStrLn "Report bugs to <blueoatstudio@gmail.com>"
