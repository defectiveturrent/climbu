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

type Tokens = [Token]

data Token
  = CONST Int
  | CONSTF Float
  | VAR
  | ID String
  | STRING String
  | CHAR Char
  | IMPORT String
  | RARROW
  | LARROW
  | PLUS
  | MINUS
  | MUL
  | DIV
  | GREATERTHAN
  | GREATEREQUAL
  | LESSTHAN
  | LESSEQUAL
  | EQUAL
  | NOT
  | MOD
  | EXPO
  | ASSIGN
  | WITH
  | COMMA
  | CALLARGS
  | OPCALLFUNC String
  | CALLALONE String
  | COMMENT
  | COUNTLIST
  | CONCATLIST
  | OPENPAREN
  | CLOSEPAREN
  | OPENBRACKETS
  | CLOSEBRACKETS
  | OPENKEYS
  | CLOSEKEYS
  | LAMBDA
  | IF
  | THEN
  | ELSE
  | ISEITHER
  | ISNEITHER
  | WHILE
  | FOR
  | DO
  | IN
  | WHEN
  | FUNC
  | TAKE
  | LISTPATTERNMATCHING
  | NULLSTRING
  | NULL
  | YADAYADA
  | VOID
  | EOF  -- End of line
  deriving (Show, Read, Eq)