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
    let (Just action) = lookup command dispatch in action args -- get the function that corresponds to argument

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
      putStrLn "Climbu compiler v0.6 - Copyright (C) 2014  Mario Feroldi"
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
