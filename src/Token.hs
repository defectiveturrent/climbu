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
  | VOID
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
  = ['+', '-', '*', '/', '^', '=', '%', '>', '<', ':', '.', ',', '|']

complexOperators
  = ['(', ')', '[', ']']

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
    , "true"
    , "false"
    , "not"
    , "either"
    , "neither"
    ]

--instance Show Token where
--  show t = reverseTokens t


showt = drawTokens

drawTokens = intercalate " " . map reverseTokens

reverseTokens e
  = case e of
      CONST n -> show n
      CONSTF n -> show n
      DECLARE -> "var"
      IDENT n -> n
      STRING n -> "\"" ++ n ++ "\""
      CHAR n -> '\'' : n : '\'' : []
      IMPORT1 -> "import"
      IMPORT n -> "import " ++ n ++ "\n"
      RARROW -> "->"
      LARROW -> "<-"
      PLUS -> "+"
      MINUS -> "-"
      MUL -> "*"
      DIV -> "/"
      GRTH -> ">"
      GRTHEQ -> ">="
      LSTH -> "<"
      LSTHEQ -> "<="
      EQUAL -> "=="
      NOT -> "/="
      MOD -> "%"
      EXP -> "^"
      AND -> "and"
      OR -> "or"
      TRUE -> "true"
      FALSE -> "false"
      ASSIGN -> "="
      WITH -> "with"
      COMMA -> ","
      CALLARGS -> "."
      OPCALLFUNCTION n -> "`" ++ n ++ "`"
      CALL1 -> "call"
      CALLALONE n -> "call " ++ n
      COMMENT -> "//"
      COUNTLIST -> ".."
      CONCATLIST -> "++"
      OPENPAREN -> "("
      CLOSEPAREN -> ")"
      OPENLIST -> "["
      CLOSELIST -> "]"
      LAMBDA -> "lam"
      IF -> "if"
      THEN -> "then"
      ELSE -> "else"
      ELSEIF -> "|"
      ISEITHER -> "either"
      ISNEITHER -> "neither"
      FOR -> "for"
      DO -> "do"
      IN -> "in"
      WHEN -> "when"
      FUNCTION -> "def"
      TAKE -> "|>"
      LISTPATTERNMATCHING -> ":"
      NULLSTRING -> "\"\""
      VOIDARGUMENTS -> "()"
      NULL -> "null"
      YADAYADA -> "..."
      VOID -> "void"
      EOF -> ";\n\n"