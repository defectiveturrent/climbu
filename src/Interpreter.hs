module Interpreter where

import Data.Char
import Data.List
import Data.Maybe
import Ast
import Parser

import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)

-- TODO:
--
-- + Concatenation between identifiers
-- + lambda
-- + Tupla

readString :: String -> [Ast]
readString str = read str :: [Ast]

saveBytecode :: [Ast] -> String -> IO ()
saveBytecode bc path
  = do
    writeFile path ("#!climbu\n" ++ show bc)

openBytecode :: String -> IO [Ast]
openBytecode path
  = do
    s <- readFile path
    return $ readString s

{-
  | Take Ast Ast                       -- list take n
  | IsEither Ast [Ast]                 -- n in either 1 2 3
  | IsNeither Ast [Ast]                 -- n in either 1 2 3
  | DoIn [Ast] Ast                     -- An expression that allows to make more expressions on a single block
  | Import String                      -- To import a library
  | ListPM [Ast] Ast                   -- A list's pattern matching operator (:) ( [Head] Tail )
  | AsCast Ast Ast                     --
  -}

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
  deriving(Eq, Read)

data Chunk = Chunk ByteString Type
           | List [Chunk] Type
           | Tuplec [Chunk] Type
           | Declaration Chunk Chunk Type
           | Functionc ByteString [Ast] Ast
           | Lambdac [Ast] Ast
           | Callf [Chunk] Chunk -- for omitting arguments on a function call (Assigned Function)
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

  show (Callf _ fn)
    = "stacked " ++ show fn

  show (Decls decls)
    = intercalate ",\n" $ map show decls


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
      eval (Ident "True") = Chunk (packShow True) BOOL
      eval (Ident "False") = Chunk (packShow False) BOOL
      eval (Ident "true") = Chunk (packShow True) BOOL
      eval (Ident "false") = Chunk (packShow False) BOOL
      eval str@(CharString _) = eval $ literaleval str

      eval (Negate (Num x)) = eval (Num (-x))
      eval (Negate (Numf x)) = eval (Numf (-x))
      eval (Negate x) = eval (Sub (Num 0) x)

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
                    Chunk (pack "False") BOOL
                  else
                    Chunk (packShow . and . zipWith (==) x $ y) BOOL
          in
            comp (eval a) (eval b)


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
            getType (Ident "True") = BOOL
            getType (Ident "False") = BOOL
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

      eval for@(For what (In (Ident n) list) Void)
        = let
            checkList
              = case list of
                  ComprehensionList _ -> let (List content _) = eval list in content
                  CountList _ _ -> let (List content _) = eval list in content
                  Ident m -> let (List content _) = getFromStack m stack in content
                  _ -> error $ "Some values are lost: " ++ show for

            mapped = map (\x -> evaluate ((Declaration (Chunk (pack n) (typeof x)) x (typeof x)) : stack) what) checkList
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

            for@(For what (In (Ident n) list) Void) ->
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