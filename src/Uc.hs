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
import Interpreter
import qualified Control.Exception as Exc

main = do hSetBuffering stdin LineBuffering
          toTry `catch` handler

catch = catchIOError

toTry :: IO ()
toTry
  = do
      args <- getArgs
      let
        command = if null args then "--REPL" else head args
        rest = if null args then [] else tail args
        (Just action) = lookup command dispatch
      
      action rest -- gets the function that corresponds to argument

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
           , "--REPL"         -- interpreter
           ]

dispatch :: [ (String, [String] -> IO ()) ]
dispatch = zip commands [ compile
                        , interpret
                        , version
                        , version
                        , help
                        , help
                        , repl
                        ]

compile (executable:pathsource:_)
  = do
      source <- readFile pathsource
      saveBytecode (parseLines . parseTokens $ source) (executable ++ ".sh")
      return ()

interpret (line:_)
  = do
      let hError :: Exc.ErrorCall -> IO ()
          hError exception = putStrLn $ show exception

      Exc.catch (print $ evaluate [] (getAst line)) hError
      hFlush stdout
      return ()

interpret (file:_)
  = do
      let
        hError :: [Chunk] -> Exc.ErrorCall -> IO ()
        hError exception
          = do putStrLn $ show exception
               putStrLn []

        sub stack
          = do source <- readFile file
               let 
                   interpreted = evaluate stack (getAst line)

               Exc.catch (print interpreted) hError
      
      sub []

version _
  = do
      putStrLn "Climbu compiler v1.7 (The seventh seal is opened) - Copyright (C) 2014 - 2015  Mario Feroldi"
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

repl _
  = do
      version [""]
      putStrLn "To exit, press CTRL + D\n"
      let
        hError :: [Chunk] -> Exc.ErrorCall -> IO ()
        hError stack exception
          = do
              putStrLn $ show exception
              putStrLn []
              hFlush stdout
              sub stack

        sub stack
          = do
              putStr "climbu> "
              hFlush stdout
              line <- getLine
              let
                interpreted = evaluate stack (getAst line)

              Exc.catch (print interpreted) (hError stack)

              let
                new = nub $ case interpreted of
                        Chunk _ _ -> stack
                        _ -> interpreted : stack

              putStrLn []
              sub $ case head new of
                (Declaration (Chunk n _) _ _) -> head new : removeIdent n (tail new)
                (Functionc n _ _) -> head new : removeIdent n (tail new)
                _ -> new
      
      sub []