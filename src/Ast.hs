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

module Ast where

type Asts = [Ast]

data Ast
  = Def Ast [Ast] Ast                  -- ID [ARGS] BODY
  | Assign Ast Ast                     -- Assign AST AST
  | Decl String                        -- var foo
  | Add Ast Ast                        -- Plus AST AST
  | Sub Ast Ast                        -- Minus AST AST
  | Mul Ast Ast                        -- Times AST AST
  | Div Ast Ast                        -- Divides AST AST
  | Okay                               -- True
  | Grt Ast Ast                        -- Greater Than AST AST
  | Ge  Ast Ast                        -- Greater or equal AST AST
  | Let Ast Ast                        -- Less Than AST AST
  | Le  Ast Ast                        -- Less or equal AST AST
  | Equ Ast Ast                        -- Equal AST AST
  | Noteq Ast Ast                      -- Not equal AST AST
  | Not Ast                            -- Not
  | Mod Ast Ast                        -- Module AST AST
  | And Ast Ast                        -- Expressive and
  | Or Ast Ast                         -- Expressive or
  | Take Ast Ast                       -- list take n
  | Expo Ast Ast                       -- Exponential AST AST
  | Concat Ast Ast                     -- Concat a list
  | CountList Ast Ast                  -- CountList Ast Ast // [0..9]
  | Ident String                       -- VAR
  | CharString String                  -- A string
  | CharByte Char                      -- A character
  | Num Int                            -- Integer number
  | Numf Float                         -- Float number
  | Tuple [Ast]                        -- (a, 7, "hello")
  | ComprehensionList [Ast]            -- ComprehensionList [DATES] // [1, 2, 3]
  | LambdaDef [Ast] Ast                -- LambdaDef [ARGS] BODY // {n = n + foo}
  | Call Ast [Ast]                     -- ID [ARGS]
  | Condition Ast Ast Ast              -- (Condition ast) (Then ast) (Else ast)
  | IsEither Ast [Ast]                 -- n in either 1 2 3
  | IsNeither Ast [Ast]                 -- n in either 1 2 3
  | In Ast Ast                         -- Simple operator, like each
  | For Ast Ast Ast                    -- Specific comprehension list
  | DoIn [Ast] Ast                     -- An expression that allows to make more expressions on a single block
  | Import String                      -- To import a library
  | Unlist [Ast]                       -- A list's pattern matching operator (:) ( [Head] Tail )
  | Negate Ast                         --
  | AsCast Ast Ast                     --
  | Some Ast                           -- Simulates the Haskell's Maybe
  | None                               -- Same above
  | Unwrap Ast                         -- Unwrap an Option
  | Void                               -- Used to stuff something empty
  | Eof                                -- used to end a complete expression
  deriving (Show, Read, Eq)
