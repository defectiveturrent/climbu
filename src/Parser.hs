{-
    Climbu compiler / interpreter
    Copyright (C) 2014 - 2015 Mario Feroldi

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

module Parser where

import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils
import Expressions
import ErrorHandler
import Token
import Ast

{-----------------------------
    String to Token Parser
-----------------------------}

{-----------------------------
    String Revision
-----------------------------}

stringRevision [] = ";"
stringRevision ('.':'.':'.':rest) = " ... " ++ stringRevision rest
stringRevision ('.':'.':rest) = " .. " ++ stringRevision rest
stringRevision (x:xs) = x : stringRevision xs

{-----------------------------
    Utils
-----------------------------}

getname :: String -> (String, String) -- (name, rest)
getname (h:str)
  | isName h = let
                 getname' [] chs = (chs, [])
                 getname' (ch : str) chs
                   | isDigitName ch = getname' str (chs++[ch])
                   | otherwise  = (chs, ch : str)

                 (name, rest) = getname' str []
               in
                 (h:name, rest)

-- Just letters and underscore
isName ch
  = ch `elem` identifiers \\ digits

-- All
isDigitName ch
  = ch `elem` identifiers

howManyTimes x xs = length $ x `elemIndices` xs

{-----------------------------
    Tokenizer's begin
-----------------------------}

tokenize :: String -> Tokens
tokenize [] = []
tokenize ('"':'"':rest) = NULLSTRING : tokenize rest

tokenize ('"':rest)
  = let
      subtokenize ('\\' : '"' : rest2) acc
        = subtokenize (rest2) (acc ++ "\"")

      subtokenize ('"' : rest2) acc
        = STRING acc : tokenize rest2

      subtokenize (x : rest2) acc
        = subtokenize (rest2) (acc ++ [x])
    in
      subtokenize rest []

tokenize ('\'':x:'\'':rest) = CHAR x : tokenize rest

tokenize ('/':'/':rest)
  = let
      (_, rest2) = break (=='\n') rest
    in
      tokenize (if null rest2 then [] else tail rest2)

tokenize ('(':'*':rest)
  = let
      subtokenize [] = []
      subtokenize ('*':')':rest2)
        = tokenize rest2

      subtokenize (_:rest2)
        = subtokenize rest2
    in
      subtokenize rest

tokenize ('`':rest)
  = let
      (n, '`':rest2) = getname rest

    in
      OPCALLFUNCTION n : tokenize rest2


tokenize (x:xs) | x `elem` whitespaces      = tokenize xs
                | x `elem` digits \\ ['.']  = doDigit [x] xs
                | x `elem` identifiers      = doIdentifier [x] xs
                | x `elem` singleOperators  = doSingleOperator [x] xs
                | x `elem` operators        = doOperator [x] xs
                | x == ';'                  = EOF : tokenize xs
                | otherwise                 = report UnknownToken (show x) (show (x:xs))
                where
                  doDigit stack (d:ds)
                    = if d `elem` digits
                        then
                          doDigit (stack ++ [d]) ds
                        else let
                                indices = howManyTimes '.' stack
                              in
                              (if indices == 1
                                then
                                  CONSTF (read stack :: Float)
                                else
                                  CONST (read stack :: Int)) : tokenize (d:ds)
                  
                  doIdentifier stack (d:ds)
                    = if d `elem` identifiers
                        then
                          doIdentifier (stack ++ [d]) ds
                        else
                          (if stack `notElem` keywords
                            then
                              IDENT stack
                            else
                              case stack of
                                "var"     -> DECLARE
                                "def"     -> FUNCTION
                                "end"     -> EOF
                                "if"      -> IF
                                "then"    -> THEN
                                "else"    -> ELSE
                                "for"     -> FOR
                                "in"      -> IN
                                "do"      -> DO
                                "null"    -> NULL
                                "call"    -> CALL1
                                "import"  -> IMPORT1
                                "when"    -> WHEN
                                "and"     -> AND
                                "or"      -> OR
                                "true"    -> TRUE
                                "false"   -> FALSE
                                "not"     -> NOT
                                "either"  -> ISEITHER
                                "neither" -> ISNEITHER
                                "try"     -> TRY
                                "match"   -> MATCH
                                "with"    -> WITH
                                "so"      -> SO
                                "as"      -> AS ) : tokenize (d:ds)
                  
                  doSingleOperator stack ds
                    = (case stack of
                        "(" -> OPENPAREN
                        ")" -> CLOSEPAREN
                        "[" -> OPENLIST
                        "]" -> CLOSELIST
                        "!" -> EXCLAMATION
                        "~" -> LAMBDA) : tokenize ds

                  doOperator stack (d:ds)
                    = if d `elem` operators
                        then
                           doOperator (stack ++ [d]) ds
                        
                        else
                          (case stack of
                            "+"   -> PLUS
                            "-"   -> MINUS
                            "*"   -> MUL
                            "/"   -> DIV
                            "="   -> ASSIGN
                            "=="  -> EQUAL
                            "/="  -> NOT
                            "%"   -> MOD
                            ">"   -> GRTH
                            ">="  -> GRTHEQ
                            "<"   -> LSTH
                            "<="  -> LSTHEQ
                            "^"   -> EXP
                            "->"  -> RARROW
                            "<-"  -> LARROW
                            ":"   -> LISTPATTERNMATCHING
                            "|"   -> ELSEIF
                            "."   -> COMPOSITION
                            ","   -> COMMA
                            "++"  -> CONCATLIST
                            ".."  -> COUNTLIST
                            "..." -> YADAYADA) : tokenize (d:ds)

tokenAdjustments :: Tokens -> Tokens
tokenAdjustments [] = []
tokenAdjustments (IMPORT1:STRING str:rest)
  = IMPORT str : tokenAdjustments rest

tokenAdjustments (ELSEIF:rest)
  = ELSE : IF : tokenAdjustments rest

tokenAdjustments (IDENT n:EXCLAMATION:rest)
  = CALLALONE n : tokenAdjustments rest

tokenAdjustments (x:xs)
  = x : tokenAdjustments xs

tokenRevision :: Tokens -> Tokens
tokenRevision [] = []

tokenRevision (LAMBDA:rest)
  = let
      (body, rest2) = break (==RARROW) rest
    in
      LAMBDA : body ++ tokenRevision rest2

tokenRevision (ISEITHER:rest)
  = let
      (body, rest2) = break (==THEN) rest
    in
      ISEITHER : body ++ tokenRevision rest2

tokenRevision (ISNEITHER:rest)
  = let
      (body, rest2) = break (==THEN) rest
    in
      ISNEITHER : body ++ tokenRevision rest2

tokenRevision (x:rest) = x : tokenRevision rest

parseTokens :: String -> Tokens
parseTokens
  = tokenRevision . tokenAdjustments . tokenize . stringRevision

{-----------------------------
      Token to Ast Parser
-----------------------------}

-- Abstraction for writting fast
type Parsed a = (a, Tokens)

-- Parses a sequence of individual expressions
parseLines :: Tokens -> Asts
parseLines [] = []
parseLines xs = let (ast, EOF:r) = parse xs in ast : parseLines r

----------------------------

----------------------------

factor :: Tokens -> Parsed Ast

factor (IDENT y : rest)
  = (Ident y, rest)

factor (TRUE : rest)
  = (Ident "True", rest)

factor (FALSE : rest)
  = (Ident "False", rest)

factor (STRING str : rest)
  = (CharString str, rest)

factor (CHAR y : rest)
  = (CharByte y, rest)

factor (CONST y : rest)
  = (Num y, rest)

factor (CONSTF y : rest)
  = (Numf y, rest)

factor (CALLALONE y : rest)
  = (Call (Ident y) [], rest)

factor (IMPORT y : rest)
  = (Import y, rest)

factor (OPENLIST : CLOSELIST : rest)
  = (ComprehensionList [], rest)

factor (OPENPAREN : CLOSEPAREN : rest)
  = (Tuple [], rest)

factor (OPENLIST : rest)
  = let
      sub stack (ast, COUNTLIST:xs)
        = let
            (y, CLOSELIST:ys) = parse xs
          in
            (CountList ast y, ys)

      sub stack (ast, COMMA:xs) = sub (stack ++ [ast]) $ parse xs
      sub stack (ast, CLOSELIST:xs) = (ComprehensionList (stack ++ [ast]), xs)
    in
      sub [] $ parse rest

factor (OPENPAREN : xs)
  = let
      sub (y, CLOSEPAREN:ys)
        = case y of
            Ident _ -> (Tuple [y], ys)
            _       -> (y, ys)

      sub (y, COMMA:ys)
        = let
            subparse stack (z, COMMA:zs) = subparse (stack ++ [z]) (parse zs)
            subparse stack (z, CLOSEPAREN:zs) = (Tuple (stack ++ [z]), zs)
          in
            subparse [y] (parse ys)
      in
        sub $ parse xs

-- factor _ = error "Unbelievably, this abstract syntax tree is not part of my knowledge"
factor _ = (Void, [EOF])

----------------------------

----------------------------

quark :: Tokens -> Parsed Ast

quark (MINUS : tokens)
  = let
      (x, xs) = quark tokens
    in
      (Negate x, xs)

quark tokens@(IDENT y : ys)
  = let
      fold stack [] = (stack, [])
      fold stack tokens
        = let
            (w, ws) = factor tokens
          in
            if w /= Void
              then
                fold (stack ++ [w]) ws
              else
                (stack, tokens)

      (z, zs) = fold [] ys
    in
      case z of
        [] -> factor tokens
        _  -> (Call (Ident y) z, zs)

quark tokens = factor tokens

----------------------------

----------------------------

proton :: Tokens -> Parsed Ast

proton tokens
  = let
      (x, xs) = quark tokens
    in
      case xs of
        (MUL : ys) ->
          let
            (z, zs) = proton ys
          in
            (Mul x z, zs)

        (DIV : ys) ->
          let
            (z, zs) = proton ys
          in
            (Div x z, zs)

        (EXP : ys) ->
          let
            (z, zs) = proton ys
          in
            (Expo x z, zs)

        _ ->
          (x, xs)

----------------------------

----------------------------

neutron :: Tokens -> Parsed Ast

neutron tokens
  = let
      (x, xs) = proton tokens
    in
      case xs of
        (PLUS : ys) ->
          let
            (z, zs) = neutron ys
          in
            (Add x z, zs)

        (MINUS : ys) ->
          let
            (z, zs) = neutron ys
          in
            (Sub x z, zs)

        (MOD : ys) ->
          let
            (z, zs) = neutron ys
          in
            (Mod x z, zs)

        (CONCATLIST : ys) ->
          let
            (z, zs) = neutron ys
          in
            (Concat x z, zs)

        _ ->
          (x, xs)

----------------------------

----------------------------

atom :: Tokens -> Parsed Ast

atom tokens
  = let
      (x, xs) = neutron tokens
    in
      case xs of
        (GRTH : ys) ->
          let
            (z, zs) = atom ys
          in
            (Grt x z, zs)

        (GRTHEQ : ys) ->
          let
            (z, zs) = atom ys
          in
            (Ge x z, zs)

        (LSTH : ys) ->
          let
            (z, zs) = atom ys
          in
            (Let x z, zs)

        (LSTHEQ : ys) ->
          let
            (z, zs) = atom ys
          in
            (Le x z, zs)

        (EQUAL : ys) ->
          let
            (z, zs) = atom ys
          in
            (Equ x z, zs)

        (NOT : ys) ->
          let
            (z, zs) = atom ys
          in
            (Not x z, zs)

        _ ->
          (x, xs)

----------------------------

----------------------------

cell :: Tokens -> Parsed Ast

cell (IF:tokens)
  = let
      (x, THEN:xs) = cell tokens
      (y, ELSE:ys) = cell xs
      (z, zs) = cell ys
    in
      (Condition x y z, zs)

cell (DO:tokens)
  = let
      sub stack (y, IN:ys)
        = let
            (z, zs) = parse ys -- uses parse for getting high precedence
          in
            (DoIn (stack ++ [y]) z, zs)

      sub stack (y, COMMA:ys)
        = sub (stack ++ [y]) $ parse ys
    in
      sub [] $ parse tokens

cell tokens
  = let
      (x, xs) = atom tokens
    in
      case xs of
        (AND : ys) ->
          let
            (z, zs) = cell ys
          in
            (And x z, zs)

        (OR : ys) ->
          let
            (z, zs) = cell ys
          in
            (Or x z, zs)

        -- For (What to do) (x in list) (when)
        (FOR : ys) ->
          let
            (ident, IN:zs) = cell ys

            checkCondition (l, WHEN:ws)
              = let
                  (c, rs) = cell ws
                in
                  (For x (ident `In` l) (c), rs)

            checkCondition (l, ws)
              = (For x (ident `In` l) (Void), ws)
          in
            checkCondition $ cell zs

        (ASSIGN : ys) ->
          let
            (z, zs) = cell ys
          in
            (Assign x z, zs)

        (LARROW : ys) ->
          let
            (z, zs) = cell ys

          in
            (Call x [Call z []], zs)

        (OPCALLFUNCTION n : ys) ->
          let
            (z, zs) = cell ys
          in
            (Call (Ident n) [x, z],  zs)

        (COMPOSITION : ys) ->
          let
            (z, zs) = cell ys
          in
            case x of
              Call n args -> (Call n (args ++ [z]), zs)
              Ident _     -> (Call x [z],  zs)

        _ ->
          (x, xs)

----------------------------

----------------------------

human tokens
  = let
      sub (Ident foo, xs)
        = let
            (y, ys) = factor xs
          in
            case (y, ys) of
              (Tuple z, ASSIGN : zs) -> (Def (Ident foo) z body, rest)
                                        where
                                          (body, rest) = cell zs

              _ ->
                cell tokens

      sub _ = cell tokens

    in
      sub $ factor tokens

----------------------------

----------------------------

parse = human
parser = human . parseTokens 