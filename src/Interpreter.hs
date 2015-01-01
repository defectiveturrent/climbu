module Interpreter where

import Data.Char
import Data.List
import Data.Maybe
import Expressions
import Token
import Ast
import Inst
import Parser
import Translator

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

readString :: String -> [Inst]
readString str = read str :: [Inst]

saveBytecode :: [Inst] -> String -> IO ()
saveBytecode bc path
  = do
    writeFile path (show bc)

openBytecode :: String -> IO [Inst]
openBytecode path
  = do
    s <- readFile path
    return (readString s)

{-
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
  | And Ast Ast                        -- Expressive and
  | Or Ast Ast                         -- Expressive or
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
  | Try Ast                            --
  | MatchWith Ast [Ast] Ast            -- match Stat with [Stats] otherwise-stat
  | AsCast Ast Ast                     --
  | Special SpecialDate                --
  | Void                               -- Used to stuff something empty
  | Eof                                -- used to end a complete expression
  -}

data Type
  = INT
  | FLOAT
  | TCHAR
  | BOOL
  | LIST Type
  deriving(Show, Eq)

data Chunk = Chunk ByteString Type deriving(Show)

readi n = read n :: Int
readf n = read n :: Float

packShow :: (Show a) => a -> ByteString
packShow = pack . show

test x = eval . fst . parseHighExp . parseTokens $! x

eval :: Ast -> Chunk

eval (Num x) = Chunk (packShow x) INT
eval (Numf x) = Chunk (packShow x) FLOAT
eval (CharByte x) = Chunk (packShow x) TCHAR
eval (Ident "true") = Chunk (packShow "true") BOOL
eval (Ident "false") = Chunk (packShow "false") BOOL
eval (CharString xs) = Chunk (packShow xs) (LIST TCHAR)

eval (Negate (Parens (Num x))) = eval (Num (-x))
eval (Negate (Parens (Numf x))) = eval (Numf (-x))

eval (Parens a) = eval a

eval (Add a b)
  = let
      (Chunk x xt) = eval a
      (Chunk y yt) = eval b

      applyadd :: Type -> Type -> Chunk
      applyadd INT INT = Chunk (packShow (readi (unpack x) + readi (unpack y))) INT
      applyadd INT FLOAT = applyadd FLOAT FLOAT
      applyadd FLOAT INT = applyadd FLOAT FLOAT
      applyadd FLOAT FLOAT = Chunk (packShow (readf (unpack x) + readf (unpack y))) FLOAT
    in
      applyadd xt yt

eval (Concat a b)
  = let
      (Chunk x xt) = eval a
      (Chunk y yt) = eval b

      applyadd :: Type -> Type -> Chunk
      applyadd (LIST TCHAR) (LIST TCHAR) = Chunk (packShow ((read (unpack x) :: String) ++ (read (unpack y) :: String))) (LIST TCHAR)
    in
      applyadd xt yt

{-
eval (Add a b) = eval a + eval b
eval (Sub a b) = eval a - eval b
eval (Mul a b) = eval a * eval b
eval (Div a b) = eval a `div` eval b
eval (Mod a b) = eval a `mod` eval b

eval (Expo a b) = eval a ^ eval b

eval (Grt a b) = if eval a > eval b  then 1 else 0
eval (Ge  a b) = if eval a >= eval b then 1 else 0
eval (Let a b) = if eval a < eval b  then 1 else 0
eval (Le  a b) = if eval a <= eval b then 1 else 0
eval (Equ a b) = if eval a == eval b then 1 else 0
eval (Not a b) = if eval a /= eval b then 1 else 0

eval (And a b) = if eval a /= 0 && eval b /= 0 then 1 else 0
eval (Or  a b) = if eval a /= 0 || eval b /= 0 then 1 else 0

eval (Parens a) = eval a

eval (Condition a b c) = if eval a == 1 then eval b else eval c
-}