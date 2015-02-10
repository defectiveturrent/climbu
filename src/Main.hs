{-
    The MIT License (MIT)
    
    Copyright (c) 2015 Mário Feroldi Filho

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
-}

module Main where

import System.Environment
import System.IO
import System.IO.Error
import System.Process
import Data.Char
import Data.Maybe
import Data.List
import Data.String.Utils
import Expressions
import Parser
import Interpreter
import qualified Control.Exception as Exc

main = toTry `catch` handler

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

          evaling acc x
            = let
                evaluated = evaluate acc x
              in
                case evaluated of
                  Decls chunks -> chunks ++ acc
                  _ -> evaluated : acc

      source <- readFile line

      Exc.catch (animated . reverse $ foldl evaling [] (parseLines $ parseTokens source)) hError
      return ()

version _
  = do
      putStrLn "Climbu v1.8 (Actors) - The MIT License (MIT)"
      putStrLn "Copyright (c) 2015 Mário Feroldi Filho"

help _
  = do
      putStrLn "Usage: climbu [option] file...\n"
      putStrLn "  -c <bin name> <file>           Compiles a code to an abstract syntax tree"
      putStrLn "  -i <file path>                 Runs a file"
      putStrLn "  {-v --version}                 Shows version"
      putStrLn "  {-h --help}                    Shows help"
      putStrLn []
      putStrLn "Report bugs to <euseinaoseithelost@gmail.com>"

repl _
  = do
      version [""]
      putStrLn "\nTo exit, press CTRL + D\n"

      standard <- readFile "standard" >>= \x -> return $ lines x
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

              Exc.catch (animated [interpreted]) (hError stack)

              let
                new = nub $ case interpreted of
                        Chunk _ _ -> stack
                        Decls chunks -> chunks ++ stack
                        _ -> interpreted : stack

              putStrLn []
              sub $ case head new of
                (Declaration (Chunk n _) _ _) -> head new : removeIdent n (tail new)
                (Functionc n _ _) -> head new : removeIdent n (tail new)
                _ -> new
      
      sub $ foldl (\acc x -> evaluate acc (getAst x) : acc) [] standard


animated [] = return ()
animated (ToPrint l@(List _ (LIST CHAR)):stack) = do putStrLn ((show l) \\ ['"', '"']); animated stack
animated (ToPrint x : stack) = do putStrLn $ show x; animated stack
animated (x:stack) = do putStrLn $ show x; animated stack