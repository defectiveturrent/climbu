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

import Data.Char
import Data.Maybe
import Data.List

type Tokens = [Token]
type Asts = [Ast]

-- Tokens data type
--
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


-- Ast ( Abstract syntax tree )
--
data Ast
  = Def Ast [Ast] Ast                  -- ID [ARGS] BODY
  | Assign Ast Ast                     -- Assign AST AST
  | Decl String                        -- var foo
  | Add Ast Ast                        -- Plus AST AST
  | Sub Ast Ast                        -- Minus AST AST
  | Mul Ast Ast                        -- Times AST AST
  | Div Ast Ast                        -- Divides AST AST
  | Grt Ast Ast                        -- Greater Than AST AST
  | Ge  Ast Ast                        -- Greater or equal AST AST
  | Let Ast Ast                        -- Less Than AST AST
  | Le  Ast Ast                        -- Less or equal AST AST
  | Equ Ast Ast                        -- Equal AST AST
  | Not Ast Ast                        -- Not equal AST AST
  | Mod Ast Ast                        -- Module AST AST
  | Take Ast Ast                       -- list take n
  | Expo Ast Ast                       -- Exponential AST AST
  | Concat Ast Ast                     -- Concat a list
  | CountList Ast Ast                  -- CountList Ast Ast // [0..9]
  | Ident String                       -- VAR
  | CharString String                  -- A string
  | CharByte Char                      -- A character
  | Num Int                            -- Integer number
  | Numf Float                         -- Float number
  | Parens Ast                         -- ( )
  | Tuple [Ast]                        -- (a, 7, "hello")
  | ComprehensionList [Ast]            -- ComprehensionList [DATES] // [1, 2, 3]
  | LambdaDef [Ast] Ast                -- LambdaDef [ARGS] BODY // {n = n + foo}
  | Call Ast [Ast]                     -- ID [ARGS]
  | Then Ast                           -- The 'then' part of 'if' block
  | Else Ast                           -- The 'else' part of 'if' block
  | Condition Ast Ast Ast              -- (Condition ast) (Then ast) (Else ast)
  | IsEither Ast [Ast]                 -- n in either 1 2 3
  | IsNeither Ast [Ast]                 -- n in either 1 2 3
  | When Ast                           -- Simple condition
  | In Ast Ast                         -- Simple operator, like each
  | For Ast Ast Ast                    -- Specific comprehension list
  | DoIn [Ast] Ast                     -- An expression that allows to make more expressions on a single block
  | Import String                      -- To import a library
  | ListPM [Ast] Ast                   -- A list's pattern matching operator (:) ( [Head] Tail )
  | Negate Ast                         --
  | Special SpecialDate                --
  | Void                               -- Used to stuff something empty
  | Eof                                -- used to end a complete expression
  deriving (Show, Read, Eq)

data SpecialDate
  = Undefined
  | NaN
  | Infinite
  | NuL
  | NuT
  | NuS
  | Null
  deriving(Show, Read, Eq, Enum)


eofers
  = [ CLOSEPAREN
    -- , CLOSEBRACKETS
    , RARROW
    , LARROW
    -- , VAR -- I don't know the reason, but it's crashing the let-in expression
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
    -- , COUNTLIST
    , EOF
    , PLUS
    , MINUS
    , MUL
    , DIV
    , GREATERTHAN
    , GREATEREQUAL
    , LESSTHAN
    , LESSEQUAL
    , EQUAL
    , NOT
    , MOD
    , EXPO
    , FUNC
    , ASSIGN
    , ISEITHER
    , ISNEITHER
    , CONCATLIST
    , YADAYADA
    ]