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

import Token
import Ast
import ErrorHandler

-- A set of all expression's limiters
eofers
  = [ CLOSEPAREN
    , CLOSELIST
    , RARROW
    , LARROW
    -- , VAR -- I don't know the reason, but it's crashing the do-in expression
    , IF
    , THEN
    , ELSE
    , DO
    , FOR
    , IN
    , WHEN
    , CALLARGS
    , COMMA
    , COMMENT
    , LISTPATTERNMATCHING
    , COUNTLIST
    , EOF
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
    ]