module Interpreter where

import Data.Char
import Data.List
import Data.Maybe
import Ast
import Parser

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

readString :: String -> [Ast]
readString str = read str :: [Ast]

saveBytecode :: [Ast] -> String -> IO ()
saveBytecode bc path
  = do
    writeFile path (show bc)

openBytecode :: String -> IO [Ast]
openBytecode path
  = do
    s <- readFile path
    return (readString s)

{-
data Ast
  = Def Ast [Ast] Ast                  -- ID [ARGS] BODY
  | Assign Ast Ast                     -- Assign AST AST
  | Decl String                        -- var foo
  | Take Ast Ast                       -- list take n
  | Ident String                       -- VAR
  | Tuple [Ast]                        -- (a, 7, "hello")
  | LambdaDef [Ast] Ast                -- LambdaDef [ARGS] BODY // {n = n + foo}
  | Call Ast [Ast]                     -- ID [ARGS]
  | IsEither Ast [Ast]                 -- n in either 1 2 3
  | IsNeither Ast [Ast]                 -- n in either 1 2 3
  | For Ast Ast Ast                    -- Specific comprehension list
  | DoIn [Ast] Ast                     -- An expression that allows to make more expressions on a single block
  | Import String                      -- To import a library
  | ListPM [Ast] Ast                   -- A list's pattern matching operator (:) ( [Head] Tail )
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
  | CHAR
  | BOOL
  | LIST Type
  | TUPLE [Type]
  deriving(Eq)

data Chunk = Chunk ByteString Type
           | List [Chunk] Type
           | Tuplec [Chunk] Type
           | Declaration Chunk Chunk Type
           deriving (Eq)

value (Chunk bs _) = unpack bs

declValue (Declaration _ chunk _) = chunk
declIdent (Declaration chunk _ _) = chunk

typeof (Chunk _ t) = t
typeof (List _ t) = t
typeof (Tuplec _ t) = t
typeof (Declaration _ _ t) = t

{--------------------------
  Don't look here,
              go away.
---------------------------}

instance Show Type where
  show INT = "Int"
  show FLOAT = "Float"
  show CHAR = "Char"
  show BOOL = "Bool"
  show (LIST r) = "[" ++ show r ++ "]"
  show (TUPLE r) = "(" ++ intercalate "," (map show r) ++ ")"

instance Show Chunk where
  show chunk@(Chunk _ t) = showChunk chunk ++ " :: " ++ show t
  show list@(List _ t) = showChunk list ++ " :: " ++ show t
  show tuple@(Tuplec _ t) = showChunk tuple ++ " :: " ++ show t
  show decl@(Declaration _ _ t) = showChunk decl ++ " :: " ++ show t

showChunk (Chunk bs _) = unpack bs

showChunk (List list (LIST CHAR))
  = show $ foldr (\(Chunk bs _) acc -> (readc $ unpack bs) : acc) [] list

showChunk (List chunks@(Chunk _ _:_) _)
  = let
      elems = map (\(Chunk bs _) -> unpack bs) chunks
    in
      "[" ++ intercalate ", " elems ++ "]"

showChunk (List list _)
  = "[" ++ intercalate ", " (map showChunk list) ++ "]"

showChunk (Tuplec chunks _)
  = "(" ++ intercalate ", " (map showChunk chunks) ++ ")"

showChunk (Declaration var chunk _)
  = showChunk var ++ " = " ++ showChunk chunk

{--------------------------
  Okay, you
          can get back now
---------------------------}

readi n = read n :: Int
readf n = read n :: Float
readb n = read n :: Bool
readc n = read n :: Char
readstr n = read n :: String
readli n = read n :: [Int]
readlf n = read n :: [Float]
readlb n = read n :: [Bool]

packShow :: (Show a) => a -> ByteString
packShow = pack . show

literaleval (CountList (Num a) (Num b)) = ComprehensionList . map Num $ [a..b]
literaleval (CountList (Numf a) (Numf b)) = ComprehensionList . map Numf $ [a..b]
literaleval (CountList (CharByte a) (CharByte b)) = ComprehensionList . map CharByte $ [a..b]
literaleval (CountList (Ident "True") (Ident "False")) = ComprehensionList [Ident "True", Ident "False"]
literaleval (CountList (Ident "False") (Ident "True")) = ComprehensionList [Ident "False", Ident "True"]
literaleval (CharString xs) = ComprehensionList $ map CharByte xs
literaleval comp@(ComprehensionList xs) = comp

test = (evaluate [] . fst . parseHighExp . parseTokens $!)

getAst = (fst . parseHighExp . parseTokens $!)

lookupIdent _ [] = Nothing
lookupIdent ident (chunk@(Declaration (Chunk n _) content t) : xs) | ident == unpack n = Just content
                                                                   | otherwise = lookupIdent ident xs

lookupIdent ident (_:xs) = lookupIdent ident xs

getFromStack ident stack
  = case lookupIdent ident stack of
      Just c -> c
      Nothing -> error $ ident ++ " doesn't exist"

removeIdent _ [] = []
removeIdent ident (chunk@(Declaration (Chunk n _) _ t) : xs) | unpack ident == unpack n = xs
                                                             | otherwise = chunk : removeIdent ident xs

removeIdent ident (x:xs) = x : removeIdent ident xs

evaluate :: [Chunk] -> Ast -> Chunk
evaluate stack
  = eval
    where
      eval :: Ast -> Chunk

      eval (Num x) = Chunk (packShow x) INT
      eval (Numf x) = Chunk (packShow x) FLOAT
      eval (CharByte x) = Chunk (packShow x) CHAR
      eval (Ident "True") = Chunk (packShow True) BOOL
      eval (Ident "False") = Chunk (packShow False) BOOL
      eval (Ident "true") = Chunk (packShow True) BOOL
      eval (Ident "false") = Chunk (packShow False) BOOL
      eval str@(CharString _) = eval $ literaleval str

      eval (Negate (Parens (Num x))) = eval (Num (-x))
      eval (Negate (Parens (Numf x))) = eval (Numf (-x))

      eval (Parens a) = eval a

      eval (Add a b)
        = let
            (Chunk x xt) = eval a
            (Chunk y yt) = eval b

            apply :: Type -> Type -> Chunk
            apply INT INT = Chunk (packShow (readi (unpack x) + readi (unpack y))) INT
            apply INT FLOAT = apply FLOAT FLOAT
            apply FLOAT INT = apply FLOAT FLOAT
            apply FLOAT FLOAT = Chunk (packShow (readf (unpack x) + readf (unpack y))) FLOAT
          in
            apply xt yt

      eval (Sub a b)
        = let
            (Chunk x xt) = eval a
            (Chunk y yt) = eval b

            apply :: Type -> Type -> Chunk
            apply INT INT = Chunk (packShow (readi (unpack x) - readi (unpack y))) INT
            apply INT FLOAT = apply FLOAT FLOAT
            apply FLOAT INT = apply FLOAT FLOAT
            apply FLOAT FLOAT = Chunk (packShow (readf (unpack x) - readf (unpack y))) FLOAT
          in
            apply xt yt

      eval (Mul a b)
        = let
            (Chunk x xt) = eval a
            (Chunk y yt) = eval b

            apply :: Type -> Type -> Chunk
            apply INT INT = Chunk (packShow (readi (unpack x) * readi (unpack y))) INT
            apply INT FLOAT = apply FLOAT FLOAT
            apply FLOAT INT = apply FLOAT FLOAT
            apply FLOAT FLOAT = Chunk (packShow (readf (unpack x) * readf (unpack y))) FLOAT
          in
            apply xt yt

      eval (Div a b)
        = let
            (Chunk x xt) = eval a
            (Chunk y yt) = eval b

            apply :: Type -> Type -> Chunk
            apply INT INT = Chunk (packShow (readi (unpack x) `div` readi (unpack y))) INT
            apply INT FLOAT = apply FLOAT FLOAT
            apply FLOAT INT = apply FLOAT FLOAT
            apply FLOAT FLOAT = Chunk (packShow (readf (unpack x) / readf (unpack y))) FLOAT
          in
            apply xt yt

      eval (Expo a b)
        = let
            (Chunk x xt) = eval a
            (Chunk y yt) = eval b

            apply :: Type -> Type -> Chunk
            apply INT INT = Chunk (packShow (readi (unpack x) ^ readi (unpack y))) INT
            apply INT FLOAT = apply FLOAT FLOAT
            apply FLOAT INT = apply FLOAT FLOAT
            apply FLOAT FLOAT = Chunk (packShow (readf (unpack x) ** readf (unpack y))) FLOAT
          in
            apply xt yt

      eval (Grt a b)
        = let
            (Chunk x xt) = eval a
            (Chunk y yt) = eval b

            apply :: Type -> Type -> Chunk
            apply INT INT = Chunk (packShow (readi (unpack x) > readi (unpack y))) BOOL
            apply INT FLOAT = apply FLOAT FLOAT
            apply FLOAT INT = apply FLOAT FLOAT
            apply FLOAT FLOAT = Chunk (packShow (readf (unpack x) > readf (unpack y))) BOOL
          in
            apply xt yt

      eval (Ge a b)
        = let
            (Chunk x xt) = eval a
            (Chunk y yt) = eval b

            apply :: Type -> Type -> Chunk
            apply INT INT = Chunk (packShow (readi (unpack x) >= readi (unpack y))) BOOL
            apply INT FLOAT = apply FLOAT FLOAT
            apply FLOAT INT = apply FLOAT FLOAT
            apply FLOAT FLOAT = Chunk (packShow (readf (unpack x) >= readf (unpack y))) BOOL
          in
            apply xt yt

      eval (Let a b)
        = let
            (Chunk x xt) = eval a
            (Chunk y yt) = eval b

            apply :: Type -> Type -> Chunk
            apply INT INT = Chunk (packShow (readi (unpack x) < readi (unpack y))) BOOL
            apply INT FLOAT = apply FLOAT FLOAT
            apply FLOAT INT = apply FLOAT FLOAT
            apply FLOAT FLOAT = Chunk (packShow (readf (unpack x) < readf (unpack y))) BOOL
          in
            apply xt yt

      eval (Le a b)
        = let
            (Chunk x xt) = eval a
            (Chunk y yt) = eval b

            apply :: Type -> Type -> Chunk
            apply INT INT = Chunk (packShow (readi (unpack x) <= readi (unpack y))) BOOL
            apply INT FLOAT = apply FLOAT FLOAT
            apply FLOAT INT = apply FLOAT FLOAT
            apply FLOAT FLOAT = Chunk (packShow (readf (unpack x) <= readf (unpack y))) BOOL
          in
            apply xt yt

      eval (Equ a b)
        = let
            (Chunk x xt) = eval a
            (Chunk y yt) = eval b

            apply :: Type -> Type -> Chunk
            apply INT INT = Chunk (packShow (readi (unpack x) == readi (unpack y))) BOOL
            apply INT FLOAT = apply FLOAT FLOAT
            apply FLOAT INT = apply FLOAT FLOAT
            apply FLOAT FLOAT = Chunk (packShow (readf (unpack x) == readf (unpack y))) BOOL
          in
            apply xt yt

      eval (Not a b)
        = let
            (Chunk x xt) = eval a
            (Chunk y yt) = eval b

            apply :: Type -> Type -> Chunk
            apply INT INT = Chunk (packShow (readi (unpack x) /= readi (unpack y))) BOOL
            apply INT FLOAT = apply FLOAT FLOAT
            apply FLOAT INT = apply FLOAT FLOAT
            apply FLOAT FLOAT = Chunk (packShow (readf (unpack x) /= readf (unpack y))) BOOL
          in
            apply xt yt

      eval (Mod a b)
        = let
            (Chunk x xt) = eval a
            (Chunk y yt) = eval b

            apply :: Type -> Type -> Chunk
            apply INT INT = Chunk (packShow (readi (unpack x) `mod` readi (unpack y))) INT
          in
            apply xt yt

      eval (Concat a b)
        = let
            (ComprehensionList x) = literaleval a
            (ComprehensionList y) = literaleval b
          in
            eval . ComprehensionList $ x ++ y

      eval comp@(ComprehensionList xs)
        = let
            getType (Num _) = INT
            getType (Numf _) = FLOAT
            getType (CharByte _) = CHAR
            getType (CharString _) = LIST CHAR
            getType (Ident "True") = BOOL
            getType (Ident "False") = BOOL
            getType (ComprehensionList (a:_)) = LIST $ getType a
            getType (CountList a _) = LIST $ getType a -- TOFIX

          in
            List (map eval xs) (getType comp)

      eval count@(CountList a b) = eval $ literaleval count

      eval (And a b)
        = let
            (Chunk x _) = eval a
            (Chunk y _) = eval b
          in
            Chunk (packShow (readb (unpack x) && readb (unpack y))) BOOL

      eval (Or a b)
        = let
            (Chunk x _) = eval a
            (Chunk y _) = eval b
          in
            Chunk (packShow (readb (unpack x) || readb (unpack y))) BOOL

      eval (Condition stat right wrong)
        = let
            (Chunk stat' _) = eval stat
          in
            if readb (unpack stat') then eval right else eval wrong

      eval (For what (In (Ident n) list) (When Void))
        = eval list

      eval (Assign (Ident n) tree)
        = Declaration (Chunk (pack n) t) evaluedTree t
          where
            evaluedTree = evaluate stack tree
            t = typeof evaluedTree

      eval (Ident n) = getFromStack n stack

      eval ast = error $ "Unknown Ast: " ++ show ast