module Expressions where

import Data.Char
import Data.Maybe
import Data.List


-- Tokens data type
--
data Token
  = CONST Int  -- Numbers, const variables, etc
  | ID String  -- Variables, etc
  | STRING String
  | CHAR Char
  | IMPORT String
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
  | LET
  | IN
  | WHEN
  | FUNC
  | TAKE
  | LISTPATTERNMATCHING
  | VOID
  | EOF  -- End of line
  deriving (Show, Read, Eq)


-- Ast ( Abstract syntax tree )
--
data Ast
  = Def Ast [Ast] Ast                  -- ID [ARGS] BODY
  | Assign Ast Ast                     -- Assign AST AST
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
  | Num Int                            -- NUMBER
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
  | LetIn [Ast] Ast                    -- An expression that allows to make more expressions in a single block
  | Import String                      -- To import a library
  | ListPM [Ast] Ast                   -- A list's pattern matching operator (:) ( [Head] Tail )
  | Void                               -- Used to stuff something empty
  | Eof                                -- used to end a complete expression
  deriving (Show, Read, Eq)
