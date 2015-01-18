{-
    Climbu compiler / interpreter
    Copyright (C) 2014 - 2015  Mario Feroldi

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
  = ['+', '-', '*', '/', '^', '=', '%', '>', '<', ':', '.', ',', '|', ':']

singleOperators
  = ['(', ')', '[', ']', '!', '~']

-- All keywords
keywords
  = [ "if"
    , "then"
    , "else"
    , "for"
    , "in"
    , "do"
    , "null"
    , "import"
    , "when"
    , "and"
    , "or"
    , "true"
    , "false"
    , "not"
    , "either"
    , "neither"
    , "as"
    ]
