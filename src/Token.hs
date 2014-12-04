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
  | ASSIGN
  | WITH
  | COMMA
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
  | VOIDENT
  | EOF  -- End of line
  deriving (Show, Read, Eq)

-- All possibles white spaces
whitespaces
  = [ ' ' , '\t' , '\r' , '\n' ]
 
-- All digits
digits
  = ['0'..'9'] ++ ['.']
 
-- All identifiers
identifiers
  = filter (/='.') $ ['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ digits
 
-- All operators
operators
  = ['+', '-', '*', '/', '^', '=', '%', '>', '<', ':', '.', ',', '|', '(', ')', '[', ']']

-- All keywords
keywords
  = [ "var"
    , "def"
    , "lam"
    , "if"
    , "then"
    , "else"
    , "for"
    , "in"
    , "do"
    , "null"
    , "call"
    , "import"
    , "when"
    , "and"
    , "or"
    , "not"
    , "either"
    , "neither"
    ]