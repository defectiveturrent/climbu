{-
    The MIT License (MIT)
    
    Copyright (c) 2015 Mário Feroldi Filho

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
-}

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
    writeFile path ("#!climbu -i \n" ++ show bc)

openBytecode :: String -> IO [Ast]
openBytecode path
  = do
    s <- readFile path
    return $ readString s

data Type
  = INT
  | FLOAT
  | CHAR
  | BOOL
  | LIST Type
  | TUPLE [Type]
  | FUNCTION
  | LAMBDA
  | GENERIC
  | OPTION Type
  deriving(Eq, Read)

data Chunk = Chunk ByteString Type
           | List [Chunk] Type
           | Tuplec [Chunk] Type
           | Declaration Chunk Chunk Type
           | Functionc ByteString [Ast] Ast
           | Lambdac [Ast] Ast
           | ToPrint Chunk
           | Somec Chunk Type
           | Nonec Type
           | Decls [Chunk]
           deriving (Eq)

value (Chunk bs _) = unpack bs

declValue (Declaration _ chunk _) = chunk
declIdent (Declaration chunk _ _) = chunk

typeof (Chunk _ t) = t
typeof (List _ t) = t
typeof (Tuplec _ t) = t
typeof (Declaration _ _ t) = t
typeof (Functionc _ _ _) = FUNCTION
typeof (Lambdac _ _) = LAMBDA
typeof (Somec _ t) = t
typeof (Nonec t) = t

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
  show FUNCTION = "Function"
  show LAMBDA = "Lambda"
  show GENERIC = "Generic"
  show (OPTION t) = "Option " ++ show t

instance Show Chunk where
  show (Chunk bs _)
    = unpack bs

  show (List list (LIST CHAR))
    = show $ foldr (\(Chunk bs _) acc -> (readc $ unpack bs) : acc) [] list

  show (List chunks@(Chunk _ _:_) _)
    = let
        elems = map (\(Chunk bs _) -> unpack bs) chunks
      in
        "[" ++ intercalate ", " elems ++ "]"

  show (List list _)
    = "[" ++ intercalate ", " (map show list) ++ "]"

  show (Tuplec list _)
    = "(" ++ intercalate ", " (map show list) ++ ")"

  show (Declaration var chunk _)
    = show var ++ " = " ++ show chunk

  show (Functionc fnname args _)
    = unpack fnname ++ ", " ++ show (length args) ++ " argument(s)"

  show (Lambdac args _)
    = "~(" ++ intercalate " " (map (\(Ident x) -> x) args) ++ ")"

  show (Decls decls)
    = intercalate ",\n" $ map show decls

  show (ToPrint chunk)
    = show chunk

  show (Somec chunk _)
    = "Some " ++ show chunk

  show (Nonec _)
    = "None"


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
literaleval (CountList (Okay) (Not Okay)) = ComprehensionList [Okay, Not Okay]
literaleval (CountList (Not Okay) (Okay)) = ComprehensionList [Not Okay, Okay]
literaleval (CharString xs) = ComprehensionList $ map CharByte xs
literaleval comp@(ComprehensionList xs) = comp
literaleval x = x

test = (evaluate [] . fst . parser $!)

getAst = (fst . parse . parseTokens $!)

lookupIdent _ [] = Nothing
lookupIdent ident (chunk@(Declaration (Chunk n _) content t) : xs) | ident == unpack n = Just content

lookupIdent ident (def@(Functionc fn args body):xs) | ident == unpack fn = Just def

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
      eval (Okay) = Chunk (packShow True) BOOL
      eval (Not Okay) = Chunk (packShow False) BOOL
      eval (Ident "True") = Chunk (packShow True) BOOL
      eval (Ident "False") = Chunk (packShow False) BOOL
      eval str@(CharString _) = eval $ literaleval str

      eval (Negate (Num x)) = eval (Num (-x))
      eval (Negate (Numf x)) = eval (Numf (-x))
      eval (Negate x) = eval (Sub (Num 0) x)

      eval (Some x)
        = let
            e = evaluate stack x
          in
            Somec e (OPTION $ typeof e)

      eval (None) = Nonec (OPTION GENERIC)

      eval (Unwrap x)
        = case evaluate stack x of
            Somec e _ -> e
            _ -> Nonec (OPTION GENERIC)

      eval (Add a b)
        = case (eval a, eval b) of
            (Chunk x xt, Chunk y yt) ->
              let
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
            comp (Chunk x xt) (Chunk y yt)
              = let
                  apply :: Type -> Type -> Chunk
                  apply INT INT = Chunk (packShow (readi (unpack x) == readi (unpack y))) BOOL
                  apply INT FLOAT = apply FLOAT FLOAT
                  apply FLOAT INT = apply FLOAT FLOAT
                  apply FLOAT FLOAT = Chunk (packShow (readf (unpack x) == readf (unpack y))) BOOL
                  apply BOOL BOOL = Chunk (packShow (readb (unpack x) == readb (unpack y))) BOOL
                in
                  apply xt yt

            comp (List x xt) (List y yt)
              = if length x /= length y
                  then
                    Chunk (packShow False) BOOL
                  else
                    Chunk (packShow . and . zipWith (==) x $ y) BOOL

            comp (Somec x _) (Somec y _)
              = comp x y

            comp (Nonec _) (Nonec _)
              = Chunk (packShow True) BOOL

            comp _ _
              = Chunk (packShow False) BOOL

          in
            comp (eval a) (eval b)


      eval (Noteq a b) = Chunk (packShow (eval (Equ a b) == Chunk (packShow False) BOOL)) BOOL

      eval (Not a) = eval (Equ a (Not Okay))

      eval (Mod a b)
        = let
            (Chunk x xt) = eval a
            (Chunk y yt) = eval b

            apply :: Type -> Type -> Chunk
            apply INT INT = Chunk (packShow (readi (unpack x) `mod` readi (unpack y))) INT
          in
            apply xt yt

      eval (Concat a b)
        = case (literaleval a, literaleval b) of
            (ComprehensionList x, ComprehensionList y) ->
              eval . ComprehensionList $ x ++ y

            (ComprehensionList x, Ident y) ->
              let
                (List listx t) = eval $ ComprehensionList x
                (List listy _) = getFromStack y stack
              in
                List (listx ++ listy) t

            (Ident x, ComprehensionList y) ->
              let
                (List listx t) = getFromStack x stack
                (List listy _) = eval $ ComprehensionList y
              in
                List (listx ++ listy) t

            _ ->
              let
                (List listx t) = evaluate stack a
                (List listy _) = evaluate stack b
              in
                List (listx ++ listy) t

      eval (ComprehensionList [])
        = List [] $ LIST GENERIC

      eval comp@(ComprehensionList xs)
        = let
            getType (Num _) = INT
            getType (Numf _) = FLOAT
            getType (CharByte _) = CHAR
            getType (CharString _) = LIST CHAR
            getType (Okay) = BOOL
            getType (Not Okay) = BOOL
            getType (ComprehensionList (a:_)) = LIST $ getType a
            getType (CountList a _) = LIST $ getType a -- TOFIX
            getType _ = LIST GENERIC

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

      eval for@(For what (In (Ident n) list) condition)
        = let
            checkList
              = case list of
                  ComprehensionList _ -> let (List content _) = eval list in content
                  CountList _ _ -> let (List content _) = eval list in content
                  Ident m -> let (List content _) = getFromStack m stack in content
                  _ -> error $ "Some values are lost: " ++ show for

            varDeclaration x = (Declaration (Chunk (pack n) (typeof x)) x (typeof x)) : stack

            mapped = [evaluate (varDeclaration x) what | x <- checkList
                     , evaluate (varDeclaration x) condition == Chunk (pack "True") BOOL
                     ]
          in
            List mapped (LIST . typeof . head $ mapped)

      eval (Assign (Ident n) tree)
        = Declaration (Chunk (pack n) t) evaluedTree t
          where
            evaluedTree = evaluate stack tree
            t = typeof evaluedTree

      eval (Assign (Unlist idents) maybeAList)
        = case literaleval maybeAList of
            (ComprehensionList asts) ->
              let
                setupAll (x:[]) elems = [Assign x (ComprehensionList elems)]
                setupAll (x:xs) (elem':elems) = Assign x elem' : setupAll xs elems
              in
                Decls . map (evaluate stack) $ setupAll idents asts

            (Ident ast) ->
              let
                (List chunks t') = evaluate stack (Ident ast)
                (LIST t) = t'

                setupAll (Ident x:[]) elems = [Declaration (Chunk (pack x) t) (List elems t') t']
                setupAll (Ident x:xs) (elem':elems) = Declaration (Chunk (pack x) t) elem' t : setupAll xs elems
              in
                Decls $ setupAll idents chunks

            for@(For what (In (Ident n) list) _) ->
              let
                (List chunks t') = evaluate stack for
                (LIST t) = t'

                setupAll (Ident x:[]) elems = [Declaration (Chunk (pack x) t) (List elems t') t']
                setupAll (Ident x:xs) (elem':elems) = Declaration (Chunk (pack x) t) elem' t : setupAll xs elems
              in
                Decls $ setupAll idents chunks


      eval (Ident n) = getFromStack n stack

      eval (Def (Ident fnname) args body)
        = Functionc (pack fnname) args body

      eval (Call (Ident "print") [args])
        = ToPrint (evaluate stack args)

      eval (Call (Ident fnname) args)
        = case getFromStack fnname stack of
              (Functionc n args' body') -> 
                let
                  zipped = zipWith (\i what -> Assign i what) args' args
                  setLocals = checkDecls $ map eval zipped

                  checkDecls [] = []
                  checkDecls (Decls x : xs) = x ++ checkDecls xs
                  checkDecls (x:xs) = x : checkDecls xs

                  inputArgsLen = length args
                  funcArgsLen = length args'
                  omittedArgs = drop inputArgsLen args'
                in
                  if funcArgsLen > inputArgsLen
                    then
                      evaluate stack (LambdaDef omittedArgs (DoIn zipped body'))
                    else
                      evaluate (setLocals ++ stack) body'

              (Lambdac args' body') ->
                let
                  zipped = zipWith (\i@(Ident n) what -> Assign i what) args' args
                  setLocals = map eval zipped

                  inputArgsLen = length args
                  funcArgsLen = length args'
                  omittedArgs = drop inputArgsLen args'
                in
                  if funcArgsLen > inputArgsLen
                    then
                      evaluate stack (LambdaDef omittedArgs (DoIn zipped body'))
                    else
                      evaluate (setLocals ++ stack) body'

      eval (Call (LambdaDef lambArgs body) args)
        = let
            setLocals = map eval $ zipWith (\i@(Ident n) what -> Assign i what) lambArgs args
          in
            evaluate (setLocals ++ stack) body

      eval (DoIn asts result)
        = evaluate (s ++ stack) result
        where
          s = checkDecls $ foldl (\acc x -> evaluate (acc ++ stack) x : acc) [] asts

          checkDecls [] = []
          checkDecls (Decls x : xs) = x ++ checkDecls xs
          checkDecls (x:xs) = x : checkDecls xs

      eval (LambdaDef args body)
        = Lambdac args body

      eval (Tuple x)
        = let
            elems = map (evaluate stack) x
          in
            Tuplec elems (TUPLE $ map typeof elems)

      eval ast = error $ "Unknown Ast: " ++ show ast