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

module Expressions where

import Data.List
import Token
import Ast
import ErrorHandler

-- A set of all expression's limiters
eofers
  = [ CLOSEPAREN
    , RARROW
    , LARROW
    , CLOSELIST
    , DECLARE -- I don't know the reason, but it's crashing the do-in expression
    , COUNTLIST
    , IF
    , THEN
    , ELSE
    , AND
    , OR
    , TRUE
    , FALSE
    , DO
    , FOR
    , IN
    , WHEN
    , COMPOSITION
    , COMMA
    , COMMENT
    , LISTPATTERNMATCHING
    , PLUS
    , MINUS
    , MUL
    , DIV
    , GRTH
    , GRTHEQ
    , LSTH
    , LSTHEQ
    , EQUAL
    , NOT
    , MOD
    , EXP
    , FUNCTION
    , ASSIGN
    , ISEITHER
    , ISNEITHER
    , CONCATLIST
    , YADAYADA
    , TRY
    , MATCH
    , WITH
    , SO
    , EXCLAMATION
    , EOF
    ]

-- All possibles white spaces
whitespaces
  = [ ' ' , '\t' , '\r' , '\n' ]
 
-- All digits
digits
  = ['0'..'9'] ++ ['.']
 
-- All identifiers
identifiers
  = (['a'..'z'] ++ ['A'..'Z'] ++ ['_'] ++ digits) \\ ['.']
 
-- All operators
operators
  = ['+', '-', '*', '/', '^', '=', '%', '>', '<', ':', '.', ',', '|']

singleOperators
  = ['(', ')', '[', ']', '!', '~']

-- All keywords
keywords
  = [ "var"
    , "def"
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
    , "try"
    , "match"
    , "with"
    , "so"
    ]


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
      MATCH -> "match"
      WITH -> "with"
      SO -> "so"
      COMMA -> ","
      COMPOSITION -> "."
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
      LAMBDA -> "~"
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
      TRY -> "try"
      EXCLAMATION -> "!"
      VOID -> "void"
      EOF -> ";\n\n"