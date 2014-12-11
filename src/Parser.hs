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

module Parser where

import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils
import Expressions
import ErrorHandler
import Token
import Ast
import Inst

{-----------------------------
    String to Token Parser
-----------------------------}

stringRevision [] = []
stringRevision ('.':'.':'.':rest) = " ... " ++ stringRevision rest
stringRevision ('.':'.':rest) = " .. " ++ stringRevision rest
stringRevision (x:xs) = x : stringRevision xs

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

-- (comments)
tokenize ('/':'/':rest)
  = let
      (_, rest2) = break (=='\n') rest

    in
      tokenize (if null rest2 then [] else tail rest2)

-- (comments)
tokenize ('(':'*':rest)
  = let
      subtokenize [] = []
      subtokenize ('*':')':rest2)
        = tokenize rest2

      subtokenize (_:rest2)
        = subtokenize rest2

    in
      subtokenize rest

tokenize ('(':')':rest)
  = VOIDARGUMENTS : tokenize rest

tokenize ('`':rest)
  = let
      (n, '`':rest2) = getname rest

    in
      OPCALLFUNCTION n : tokenize rest2

tokenize (x:xs)   | x `elem` whitespaces      = tokenize xs
                  | x `elem` digits \\ ['.']  = doDigit [x] xs
                  | x `elem` identifiers      = doIdentifier [x] xs
                  | x `elem` complexOperators = doComplexOperator [x] xs
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
                                  "var"    -> DECLARE
                                  "def"    -> FUNCTION
                                  "lam"    -> LAMBDA
                                  "if"     -> IF
                                  "then"   -> THEN
                                  "else"   -> ELSE
                                  "for"    -> FOR
                                  "in"     -> IN
                                  "do"     -> DO
                                  "null"   -> NULL
                                  "call"   -> CALL1
                                  "import" -> IMPORT1
                                  "when"   -> WHEN
                                  "and"    -> AND
                                  "or"     -> OR
                                  "true"   -> TRUE
                                  "false"  -> FALSE
                                  "not"    -> NOT
                                  "either" -> ISEITHER
                                  "neither" -> ISNEITHER ) : tokenize (d:ds)
                    
                    doComplexOperator stack ds
                      = (case stack of
                          "(" -> OPENPAREN
                          ")" -> CLOSEPAREN
                          "[" -> OPENLIST
                          "]" -> CLOSELIST) : tokenize ds

                    doOperator stack (d:ds)
                      = if d `elem` operators
                          then
                             doOperator (stack ++ [d]) ds
                          
                          else
                            (case stack of
                              "+" -> PLUS
                              "-" -> MINUS
                              "*" -> MUL
                              "/" -> DIV
                              "=" -> ASSIGN
                              "==" -> EQUAL
                              "/=" -> NOT
                              "%" -> MOD
                              ">" -> GRTH
                              ">=" -> GRTHEQ
                              "<" -> LSTH
                              "<=" -> LSTHEQ
                              "^" -> EXP
                              "->" -> RARROW
                              "<-" -> LARROW
                              ":" -> LISTPATTERNMATCHING
                              "|" -> ELSEIF
                              "." -> CALLARGS
                              "," -> COMMA
                              "++" -> CONCATLIST
                              ".." -> COUNTLIST
                              "..." -> YADAYADA) : tokenize (d:ds)

tokenRevision :: Tokens -> Tokens
tokenRevision [] = []
tokenRevision al@(DECLARE:IDENT x:some:rest)
  = case some of
      ASSIGN -> DECLARE : IDENT x : ASSIGN : tokenRevision rest
      _ -> report BadVarDeclaration (showt [DECLARE,IDENT x, some]) (showt al)

tokenRevision (IDENT x:OPCALLFUNCTION n:rest)
  = IDENT x : OPCALLFUNCTION n : tokenRevision rest

tokenRevision (ELSEIF:rest)
  = ELSE : IF : tokenRevision rest

tokenRevision (IMPORT1:IDENT n:rest)
  = IMPORT n : tokenRevision rest

tokenRevision (CALL1:IDENT n:rest)
  = tokenRevision $ CALLALONE n : rest

tokenRevision (FUNCTION:rest)
  = let
      (body, rest2) = break (==ASSIGN) rest
    in
      FUNCTION : body ++ tokenRevision rest2

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

tokenRevision (CLOSEPAREN:IDENT x:rest)
  = CLOSEPAREN : MUL : IDENT x : tokenRevision rest

tokenRevision (CLOSEPAREN:CONST x:rest)
  = CLOSEPAREN : MUL : CONST x : tokenRevision rest

tokenRevision (CONST x:OPENPAREN:rest)
  = CONST x : MUL : OPENPAREN : tokenRevision rest

tokenRevision (CONST x:IDENT y:rest)
  = CONST x : MUL : IDENT y : tokenRevision rest

tokenRevision (CONSTF x:OPENPAREN:rest)
  = CONSTF x : MUL : OPENPAREN : tokenRevision rest

tokenRevision (CONSTF x:IDENT y:rest)
  = CONSTF x : MUL : IDENT y : tokenRevision rest

--tokenRevision ((IDENT x):OPENPAREN:rest)
--  = IDENT x : CALLARGS : OPENPAREN : tokenRevision rest

tokenRevision (IDENT x:OPENLIST:rest)
  = IDENT x : CALLARGS : OPENLIST : tokenRevision rest

tokenRevision (IDENT x:VOIDARGUMENTS:ASSIGN:rest)
  = tokenRevision (FUNCTION : IDENT x : ASSIGN : rest)

tokenRevision (IDENT x:OPENPAREN:rest)
  = formuled
  where
    isThereSomethingWrong [] = False
    isThereSomethingWrong what
      = foldr
        (\x acc -> case x of
             IDENT _ -> acc
             _ -> True )
        False
        what

    formuled = if isThereSomethingWrong args
                then
                  IDENT x : CALLARGS : OPENPAREN : tokenRevision rest
                else
                  tokenRevision (FUNCTION : IDENT x : (args ++ as))

    (args, CLOSEPAREN:as) = break (==CLOSEPAREN) rest

tokenRevision ((IDENT x):rest)
  = let
      (arguments, rest2) = getUntilEofer ([], rest)

      getUntilEofer :: (Tokens, Tokens) -> (Tokens, Tokens)
      getUntilEofer (acc, [])
        = (acc, [])
        
      getUntilEofer (acc, x:xs) | x `elem` eofers = (acc, x:xs)
                                | otherwise       = getUntilEofer (acc ++ [x], xs)

    in
      if null arguments
        then
          (IDENT x) : tokenRevision rest
        else
          [OPENPAREN, IDENT x, CALLARGS] ++ arguments ++ [CLOSEPAREN] ++ tokenRevision rest2

tokenRevision (STRING a : STRING b : rest)
  = STRING a : CONCATLIST : tokenRevision (STRING b : rest)

tokenRevision (x:rest) = x : tokenRevision rest

parseTokens :: String -> Tokens
parseTokens
  = tokenRevision . tokenize . stringRevision

applyTwice f = f . f

{-----------------------------
      Token to Ast Parser
-----------------------------}

parseEofs :: Tokens -> Either String Asts
parseEofs [] = Right []
parseEofs tokens
  = let
      (pretokens, rest) = break (==EOF) tokens
      (exptree, exprest) = parseHighExp pretokens

    in
      if null exprest
        then
          if null rest
            then
              Right [exptree]
            else
              case (parseEofs (tail rest)) of
                Right x
                  -> Right (exptree : x)
                
                Left msg
                  -> Left msg

      else
        Left $ mkmsg ExcessRubbish (showt exprest) (showt pretokens)


parseFactors :: Tokens -> (Ast, Tokens)
parseFactors (DECLARE:(IDENT x):rest)
  = (Decl x, rest)

-- Parse idents
parseFactors ((IDENT x):rest)
  = (Ident x, rest)

parseFactors (TRUE:rest)
  = (Ident "true", rest)

parseFactors (FALSE:rest)
  = (Ident "false", rest)

parseFactors ((STRING str):rest)
  = (CharString str, rest)

parseFactors ((CHAR ch):rest)
  = (CharByte ch, rest)

-- Parse numbers
parseFactors ((CONST x):rest)
  = (Num x, rest)

parseFactors ((CONSTF x):rest)
  = (Numf x, rest)

parseFactors (YADAYADA:rest)
  = (Call (Ident "alert") [CharString "Not yet implemented"], rest)

parseFactors ((CALLALONE x):rest)
  = (Call (Ident x) [], rest)

parseFactors ((IMPORT x):rest)
  = (Import x, rest)

parseFactors ((NULLSTRING):rest)
  = (Special NuS, rest)

parseFactors ((NULL):rest)
  = (Special Null, rest)

parseFactors (OPENPAREN:CLOSEPAREN:rest)
  = (Special NuT, rest)

parseFactors (OPENPAREN:MINUS:rest)
  = let
      factor = OPENPAREN : rest
      (parsed, rest2) = parseFactors factor

    in
      (Negate parsed, rest2)

-- Parse parentheses
parseFactors al@(OPENPAREN:rest)
  = let
      parseXpr (xpr, CLOSEPAREN:rest2)
        = (Parens xpr, rest2)

      parseXpr (xpr, COMMA:rest2)
        = let
            subparse stack (expr, COMMA:xs) = subparse (stack ++ [expr]) (parseHighExp xs)
            subparse stack (expr, CLOSEPAREN:xs) = (Tuple (stack ++ [expr]), xs)
          in
            subparse [xpr] (parseHighExp rest2)

      parseXpr (xpr, r) = report BadMethod "Lost parentheses" (showt al)
    in
      astRevision . parseXpr . parseHighExp $ rest

parseFactors al@(OPENLIST:CLOSELIST:rest)
  = report BadList "Empty list can't exists" (showt al)

parseFactors al@(OPENLIST:rest)
  = let
      -- Comprehension list
      parseXpr (xpr, COUNTLIST:rest2)
        = let
            (e, CLOSELIST:r) = parseHighExp rest2
          in
            (CountList xpr e, r)

      -- Common list
      parseXpr (xpr, COMMA:rest2)
        = let
            subparse stack (expr, COMMA:xs) = subparse (stack ++ [expr]) (parseHighExp xs)
            subparse stack (expr, CLOSELIST:xs) = (ComprehensionList (stack ++ [expr]), xs)
          in
            subparse [xpr] (parseHighExp rest2)

      -- Just one element
      parseXpr (xpr, CLOSELIST:rest2)
        = (ComprehensionList [xpr], rest2)

      parseXpr (xpr, r) = report BadList "Lost brackets" (showt al)
    in
      astRevision . parseXpr . parseHighExp $ rest

-- If expression
parseFactors (IF:rest)
  = let
      (stat, restStat) = parseHighExp rest
      (Then thenStat, restThen) = getExp $ parseHighExp restStat
      (Else elseStat, restElse) = getExp $ parseHighExp restThen

      getExp (Then stat, r) = (Then stat, r)
      getExp (Else Eof, r) = report IncompleteIfBlock (showt (IF:rest)) (showt [ELSE, EOF])
      getExp (Else (Else e), r) = report BadMethod (showt (IF:rest)) (showt [ELSE,ELSE])
      getExp (Else stat, r) = (Else stat, r)
      getExp (e, r) = report IncompleteIfBlock (showt (IF:rest)) (show e)
    
    in
      (Condition stat thenStat elseStat, restElse)

  -- IF Expression
  --
parseFactors ((THEN):rest)
  = let
      (stat, rest2) = parseHighExp rest

    in
     (Then stat, rest2)

  -- IF Expression
  --
parseFactors ((ELSE):rest)
  = let
      (stat, rest2) = parseHighExp rest

    in
      (Else stat, rest2)

parseFactors ((WHEN):rest)
  = let
      (stat, rest2) = parseHighExp rest

    in
      (When stat, rest2)

-- Parse end of
parseFactors ((EOF):rest)
  = (Eof, rest)

parseFactors tokens = report SyntaxError (showt tokens) []

parseFactor :: Token -> Ast
parseFactor token
  = fst $ parseFactors [token]

parseAllFactors :: Tokens -> (Asts, Tokens)
parseAllFactors tokens'
  = let
      parsef' (ast, tokens)
        = let
            (nast, rest) = parseHighExp tokens

            nextToken = if null rest then VOID else head rest
          in
            if (nextToken /= VOID) && not (nextToken `elem` eofers)
              then
                parsef' (ast ++ [nast], rest)

              else
                (ast ++ [nast], rest)
     in
      parsef' ([], tokens')

checkComma :: Tokens -> Tokens
checkComma tokens
  = (if (not $ null tokens) && (head tokens == COMMA) then tail else id) tokens

parseEachFactor :: Tokens -> (Asts, Tokens)
parseEachFactor tokens'
  = let
      parsef' (ast, tokens)
        = let
            (nast, rest) = parseFactors tokens

            nextToken = if null rest then VOID else head rest
          in
            if (nextToken /= VOID) && not (nextToken `elem` eofers)
              then
                parsef' (ast ++ [nast], rest)

              else
                (ast ++ [nast], rest)
     in
      parsef' ([], tokens')

parseHighExp :: Tokens -> (Ast, Tokens)
parseHighExp []
  = (Eof, [])

parseHighExp tokens@(_:[])
  = parseFactors tokens

parseHighExp tokens@( prefixToken : restTokens )
  = case prefixToken of
      FUNCTION ->
        let
          (stat : rest) = restTokens -- Separates tokens in prefix, stat and rest

          functionIdent = parseFactor stat
          arguments = fst . parseAllFactors . takeWhile (/=ASSIGN) $ rest
          (bodyFunction, rest2) = parseHighExp . tail . dropWhile (/=ASSIGN) $ rest -- Remove Assign from head (tail)
            
        in
          (Def functionIdent arguments bodyFunction, rest2)

      LAMBDA ->
        let
          arguments = if head restTokens == RARROW then [] else fst . parseAllFactors . takeWhile (/=RARROW) $ restTokens
          (bodyFunction, rest2) = parseHighExp . tail . getExp . dropWhile (/=RARROW) $ restTokens -- Remove arrow from head (tail)

          getExp [] = report BadLambdaDeclaration (showt tokens) []
          getExp rt = rt

        in
          (LambdaDef arguments bodyFunction, rest2)

      DO ->
        let
          (declarations, IN:rest) = getExp $ parseAllFactors restTokens
          (todo, rest2) = parseHighExp rest

          getExp (d, IN:r) = (d, IN:r)
          getExp (d, r) = report IncompleteDoBlock (showt tokens) (showt r)
        
        in
          (DoIn declarations todo, rest2)

      othertokens ->
        parseExp tokens

parseExp :: [Token] -> (Ast, [Token])
parseExp [] = (Eof, [])
parseExp tokens
  = let
      (factortree, rest) = parseFactors tokens

    in
     astRevision $ case rest of
        (FOR : rest2) ->
          let
            (varin, IN:rest3) = parseFactors rest2
            (content, rest4) = parseFactors rest3
            counter = In varin content

            (whenStat, finalRest) = getWhenStat rest4

            getWhenStat (WHEN:tokens)
              = let
                  (stat, restTokens) = parseHighExp tokens
                in
                  (When stat, restTokens)

            getWhenStat tokens
              = (When Void, tokens)

          in
            (For factortree counter whenStat, finalRest)

        (ASSIGN : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Assign factortree subexptree, checkComma rest3 )

        (PLUS : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Add factortree subexptree, rest3 )

        (MINUS : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Sub factortree subexptree, rest3 )

        (MUL : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Mul factortree subexptree, rest3 )

        (DIV : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( (case subexptree of
                            Num 0 ->
                              Special Infinite

                            _ ->
                              Div factortree subexptree
                        ), checkComma rest3 )

        (GRTH : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Grt factortree subexptree, rest3 )

        (GRTHEQ : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Ge factortree subexptree, rest3 )

        (LSTH : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Let factortree subexptree, rest3 )

        (LSTHEQ : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Le factortree subexptree, rest3 )

        (EQUAL : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision( Equ factortree subexptree, rest3 )
        
        (NOT : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Not factortree subexptree, rest3 )

        (MOD : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Mod factortree subexptree, rest3 )

        (AND : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2

          in
            astRevision (And factortree subexptree, rest3)

        (OR : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2

          in
            astRevision (Or factortree subexptree, rest3)

        (WITH : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Call factortree [subexptree], checkComma rest3 )

        (CALLARGS : rest2) ->
          let
            (subexptree, rest3) = parseAllFactors rest2
          in
            astRevision ( Call factortree subexptree, checkComma rest3 )

        (TAKE : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Take factortree subexptree, rest3 )

        (EXP : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Expo factortree subexptree, rest3 )

        (CONCATLIST : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Concat factortree subexptree, rest3 )

        (OPCALLFUNCTION n : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Call (Ident n) [factortree, subexptree], checkComma rest3 )

        (ISEITHER : rest2) ->
          let
            (subexptree, rest3) = parseEachFactor rest2

          in
            astRevision ( IsEither factortree subexptree, rest3 )

        (ISNEITHER : rest2) ->
          let
            (subexptree, rest3) = parseEachFactor rest2

          in
            astRevision ( IsNeither factortree subexptree, rest3 )

        (LARROW : rest2) ->
          let
            (subexptree, rest3) = parseFactors rest2

          in
            astRevision (Call factortree [Call subexptree []], rest3)

        -- Like an 'otherwise'
        othertokens ->   -- TODO
          astRevision (factortree, othertokens)


astRevision (Parens (Parens ast), tokens) = concept (Parens ast, tokens)
astRevision (ComprehensionList [CountList pa pb], tokens) = concept (CountList pa pb, tokens)

astRevision (Mul ta (Add pa pb), tokens) = concept (Add (Mul ta pa) pb, tokens)
astRevision (Mul ta (Sub pa pb), tokens) = concept (Sub (Mul ta pa) pb, tokens)
astRevision (Mul ta (Div pa pb), tokens) = concept (Div (Mul ta pa) pb, tokens)
astRevision (Mul ta (Grt pa pb), tokens) = concept (Grt (Mul ta pa) pb, tokens)
astRevision (Mul ta (Ge  pa pb), tokens) = concept (Ge  (Mul ta pa) pb, tokens)
astRevision (Mul ta (Let pa pb), tokens) = concept (Let (Mul ta pa) pb, tokens)
astRevision (Mul ta (Le  pa pb), tokens) = concept (Le  (Mul ta pa) pb, tokens)
astRevision (Mul ta (Equ pa pb), tokens) = concept (Equ (Mul ta pa) pb, tokens)
astRevision (Mul ta (Not pa pb), tokens) = concept (Not (Mul ta pa) pb, tokens)
astRevision (Mul ta (Mod pa pb), tokens) = concept (Mod (Mul ta pa) pb, tokens)
astRevision (Mul ta (Concat pa pb), tokens) = concept (Concat (Mul ta pa) pb, tokens)

astRevision (Div ta (Add pa pb), tokens) = concept (Add (Div ta pa) pb, tokens)
astRevision (Div ta (Sub pa pb), tokens) = concept (Sub (Div ta pa) pb, tokens)
astRevision (Div ta (Grt pa pb), tokens) = concept (Grt (Div ta pa) pb, tokens)
astRevision (Div ta (Ge  pa pb), tokens) = concept (Ge  (Div ta pa) pb, tokens)
astRevision (Div ta (Let pa pb), tokens) = concept (Let (Div ta pa) pb, tokens)
astRevision (Div ta (Le  pa pb), tokens) = concept (Le  (Div ta pa) pb, tokens)
astRevision (Div ta (Equ pa pb), tokens) = concept (Equ (Div ta pa) pb, tokens)
astRevision (Div ta (Not pa pb), tokens) = concept (Not (Div ta pa) pb, tokens)
astRevision (Div ta (Mod pa pb), tokens) = concept (Mod (Div ta pa) pb, tokens)
astRevision (Div ta (Expo pa pb), tokens) = concept (Expo (Div ta pa) pb, tokens)
astRevision (Div ta (Concat pa pb), tokens) = concept (Concat (Div ta pa) pb, tokens)

astRevision (Expo ta (Add pa pb), tokens) = concept (Add (Expo ta pa) pb, tokens)
astRevision (Expo ta (Sub pa pb), tokens) = concept (Sub (Expo ta pa) pb, tokens)
astRevision (Expo ta (Mul pa pb), tokens) = concept (Mul (Expo pa ta) pb, tokens)
astRevision (Expo ta (Div pa pb), tokens) = concept (Div (Expo ta pa) pb, tokens)
astRevision (Expo ta (Grt pa pb), tokens) = concept (Grt (Expo ta pa) pb, tokens)
astRevision (Expo ta (Ge  pa pb), tokens) = concept (Ge  (Expo ta pa) pb, tokens)
astRevision (Expo ta (Let pa pb), tokens) = concept (Let (Expo ta pa) pb, tokens)
astRevision (Expo ta (Le  pa pb), tokens) = concept (Le  (Expo ta pa) pb, tokens)
astRevision (Expo ta (Equ pa pb), tokens) = concept (Equ (Expo ta pa) pb, tokens)
astRevision (Expo ta (Not pa pb), tokens) = concept (Not (Expo ta pa) pb, tokens)
astRevision (Expo ta (Mod pa pb), tokens) = concept (Mod (Expo ta pa) pb, tokens)
astRevision (Expo ta (Concat pa pb), tokens) = concept (Concat (Expo ta pa) pb, tokens)

astRevision pair = concept pair


{-----------------------------
           Concept
-----------------------------}


concept (Add _ (CharString _), _) = report BadMethod "You can't do math between numbers and strings" "n + \"string\""
concept (Add (CharString _) _, _) = report BadMethod "You can't do math between numbers and strings" "\"string\" + n"
concept (Sub _ (CharString _), _) = report BadMethod "You can't do math between numbers and strings" "n - \"string\""
concept (Sub (CharString _) _, _) = report BadMethod "You can't do math between numbers and strings" "\"string\" - n"
concept (Mul _ (CharString _), _) = report BadMethod "You can't do math between numbers and strings" "n * \"string\""
concept (Mul (CharString _) _, _) = report BadMethod "You can't do math between numbers and strings" "\"string\" * n"
concept (Div _ (CharString _), _) = report BadMethod "You can't do math between numbers and strings" "n / \"string\""
concept (Div (CharString _) _, _) = report BadMethod "You can't do math between numbers and strings" "\"string\" / n"

concept (Add _ (ComprehensionList _), _) = report BadMethod "You can't do math between numbers and strings" "n + []"
concept (Add (ComprehensionList _) _, _) = report BadMethod "You can't do math between numbers and strings" "[] + n"
concept (Sub _ (ComprehensionList _), _) = report BadMethod "You can't do math between numbers and strings" "n - []"
concept (Sub (ComprehensionList _) _, _) = report BadMethod "You can't do math between numbers and strings" "[] - n"
concept (Mul _ (ComprehensionList _), _) = report BadMethod "You can't do math between numbers and strings" "n * []"
concept (Mul (ComprehensionList _) _, _) = report BadMethod "You can't do math between numbers and strings" "[] * n"
concept (Div _ (ComprehensionList _), _) = report BadMethod "You can't do math between numbers and strings" "n / []"
concept (Div (ComprehensionList _) _, _) = report BadMethod "You can't do math between numbers and strings" "[] / n"

concept (Concat a (Num _), _) = report BadMethod "You can just concat lists" "[] ++ n"
concept (Concat a (Numf _), _) = report BadMethod "You can just concat lists" "[] ++ n"
concept (Concat (Num _) b, _) = report BadMethod "You can just concat lists" "n ++ []"
concept (Concat (Numf _) b, _) = report BadMethod "You can just concat lists" "n ++ []"

concept (Assign a b, tokens)
  = case a of
      Num x ->
        report BadMethod "You can't assign values to numbers" (show x ++ " = ...")

      Numf x ->
        report BadMethod "You can't assign values to numbers" (show x ++ " = ...")

      _ ->
        (Assign a b, tokens)

concept ast = ast

{-----------------------------
   Ast to Instruction Parser
-----------------------------}

parseAst (Void) = TNothing
parseAst (Decl x) = DeclVar x
parseAst (Ident "_") = Ignore
parseAst (Ident x) = PushVar x
parseAst (CharString x) = CallFunction (PushVar "mkstr") [PushString x]
parseAst (CharByte x) = PushChar x
parseAst (Num x) = PushConst (show x)
parseAst (Numf x) = PushConstf (show x ++ "f")
parseAst (Assign e1 e2) = AssignTo (parseAst e1) (parseAst e2)
parseAst (Take e1 e2) = DoTake (parseAst e1) (parseAst e2)
parseAst (Expo e1 e2) = CallFunction (PushVar "pow") [parseAst e1, parseAst e2]
parseAst (Concat e1 e2) = ConcatList (parseAst e1) (parseAst e2)
parseAst (Import e1) = ImportInst $ (map (\x -> if x == '.' then '/' else x) e1) ++ ".hpp"
parseAst (Negate e1) = NegateInst $ parseAst e1

parseAst (Add e1 e2) = Operation "+" (parseAst e1) (parseAst e2)
parseAst (Sub e1 e2) = Operation "-" (parseAst e1) (parseAst e2)
parseAst (Mul e1 e2) = Operation "*" (parseAst e1) (parseAst e2)
parseAst (Div e1 e2) = Operation "/" (parseAst e1) (parseAst e2)
parseAst (Mod e1 e2) = Operation "%" (parseAst e1) (parseAst e2)
parseAst (Grt e1 e2) = Operation ">" (parseAst e1) (parseAst e2)
parseAst (Let e1 e2) = Operation "<" (parseAst e1) (parseAst e2)

parseAst (Equ e1 e2) = Operation "==" (parseAst e1) (parseAst e2)
parseAst (Not e1 e2) = Operation "!=" (parseAst e1) (parseAst e2)
parseAst (Ge  e1 e2) = Operation ">=" (parseAst e1) (parseAst e2)
parseAst (Le  e1 e2) = Operation "<=" (parseAst e1) (parseAst e2)

parseAst (And e1 e2) = AndInst (parseAst e1) (parseAst e2)
parseAst (Or e1 e2) = OrInst (parseAst e1) (parseAst e2)

parseAst (In e1 e2) = Range (parseAst e1) (parseAst e2)
parseAst (For e1 e2 (When e3))
  = let
      ranges = parseAst e2

      var' = (\(Range x _) -> x) ranges
      range' = (\(Range _ l) -> l) ranges
      result' = parseAst e1
      condition' = case e3 of
                    Void -> PushConst "true"
                    _ -> parseAst e3

    in
      ForList (Lambda [var'] result') range' (Lambda [var'] condition')

parseAst (CountList e1 e2) = MakeCountList (parseAst e1) (parseAst e2)
parseAst (ComprehensionList xs) = MakeSimpleList (map parseAst xs)
parseAst (Parens e1) = Block (parseAst e1)
parseAst (Def name args body) = Function (parseAst name) (map parseAst args) (parseAst body)
parseAst (LambdaDef args body) = Lambda (map parseAst args) (parseAst body)
parseAst (Call name args) = CallFunction (parseAst name) (map parseAst args)
parseAst (Condition stat thenStat elseStat) = Block $ MakeCondition (parseAst stat) (parseAst thenStat) (parseAst elseStat)
parseAst (IsEither e1 e2) = CallFunction (PushVar "elem") [parseAst e1, MakeSimpleList $ map parseAst e2]
parseAst (IsNeither e1 e2) = Block $ CallFunction (PushVar "!elem") [parseAst e1, MakeSimpleList $ map parseAst e2]
parseAst (DoIn e1 e2) = DoStack $ map parseAst (e1 ++ [e2])
parseAst (Tuple e) = TupleInst $ map parseAst e
parseAst (ListPM e1 e2) = ListPMInst (map parseAst e1) (parseAst e2)
parseAst (Special x) = PushVar $ show x
parseAst _ = TNothing