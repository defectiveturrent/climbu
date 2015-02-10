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

module Token where

import Data.List
import ErrorHandler

type Tokens = [Token]

data Token
  = CONST Int
  | CONSTF Float
  | DECLARE
  | IDENT String
  | STRING String
  | CHAR Char
  | IMPORT1
  | IMPORT String
  | RARROW
  | LARROW
  | PLUS
  | MINUS
  | MUL
  | DIV
  | GRTH
  | GRTHEQ
  | LSTH
  | LSTHEQ
  | EQUAL
  | NOTEQ
  | NOT
  | MOD
  | EXP
  | AND
  | OR
  | TRUE
  | FALSE
  | ASSIGN
  | MATCH
  | WITH
  | SO
  | AS
  | COMMA
  | EXCLAMATION
  | COMPOSITION
  | OPCALLFUNCTION String
  | CALL1
  | CALLALONE String
  | COMMENT
  | COUNTLIST
  | CONCATLIST
  | OPENPAREN
  | CLOSEPAREN
  | OPENLIST
  | CLOSELIST
  | LAMBDA
  | IF
  | THEN
  | ELSE
  | ELSEIF
  | ISEITHER
  | ISNEITHER
  | FOR
  | DO
  | IN
  | WHEN
  | FUNCTION
  | TAKE
  | COLON
  | NULLSTRING
  | VOIDARGUMENTS
  | NULL
  | YADAYADA
  | TRY
  | VOID
  | EOF  -- End of line
  deriving (Show, Read, Eq)

