module Expressions where

import Data.Char
import Data.Maybe
import Data.List


-- Tokens data type
--
data Token
  = CONST Float  -- Numbers, const variables, etc
  | ID String  -- Variables, etc
  | PLUS
  | MINUS
  | MUL
  | DIV
  | GREATERTHAN
  | LESSTHAN
  | EQUAL
  | MOD
  | ASSIGN
  | EACH
  | WITH
  | COMMA
  | CALLARGS
  | COMMENT
  | COUNTLIST
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
  | WHILE
  | FOR
  | FUNC
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
  | Let Ast Ast                        -- Less Than AST AST
  | Equ Ast Ast                        -- Equal AST AST
  | Mod Ast Ast                        -- Module AST AST
  | Each Ast Ast                       -- Each AST AST
  | CountList Ast Ast                  -- CountList Ast Ast // [0..9]
  | Ident String                       -- VAR
  | Num Float                          -- NUMBER
  | ParenthesesBlock Ast               -- ( )
  | ComprehensionList [Ast]            -- ComprehensionList [DATES] // [1, 2, 3]
  | LambdaDef [Ast] Ast                -- LambdaDef [ARGS] BODY // {n = n + foo}
  | Call Ast [Ast]                     -- ID [ARGS]
  | Then Ast
  | Else Ast
  | Condition Ast Ast Ast              -- (Condition ast) (Then ast) (Else ast)
  | Eof
  deriving (Show, Read, Eq)
