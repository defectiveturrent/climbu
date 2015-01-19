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

module ErrorHandler where

data ExpException
  = IncompleteOperation
  | IncompleteDoBlock
  | IncompleteIfBlock
  | BadList
  | BadMethod
  | BadFunctionDeclaration
  | BadVarDeclaration
  | BadLambdaDeclaration
  | UnknownToken
  | ExcessRubbish
  | SyntaxError
  deriving(Show, Read, Eq)


showEE IncompleteOperation msg = "Incomplete operation: " ++ msg
showEE IncompleteDoBlock msg = "Incomplete do-in: " ++ msg
showEE IncompleteIfBlock msg = "Incomplete if-block: " ++ msg
showEE BadList msg = "Bad list: " ++ msg
showEE BadMethod msg = "Bad method: " ++ msg
showEE BadFunctionDeclaration msg = "Bad function declaration: " ++ msg
showEE BadVarDeclaration msg = "Bad variable declaration: " ++ msg
showEE BadLambdaDeclaration msg = "Bad lambda declaration: " ++ msg
showEE UnknownToken msg = "Unknown token: " ++ msg
showEE ExcessRubbish msg = "Excess rubbish on stack: "
showEE SyntaxError msg = "Unknown syntax: " ++ msg

report :: ExpException -> String -> String -> t
report e al wh = error $ mkmsg e al wh

mkmsg e al wh = showEE e (al ++ "\n\tat: " ++ wh ++ "\n")