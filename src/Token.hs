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
  | COMMA
  | EXCLAMATION
  | CALLARGS
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
  | LISTPATTERNMATCHING
  | NULLSTRING
  | VOIDARGUMENTS
  | NULL
  | YADAYADA
  | TRY
  | VOID
  | EOF  -- End of line
  deriving (Show, Read, Eq)

