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
import Token
import Ast
import Inst

{-----------------------------
    String to Token Parser
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

tokenize ('`':rest)
  = let
      (n, '`':rest2) = getname rest

    in
      OPCALLFUNCTION n : tokenize rest2

tokenize (x:xs)   | x `elem` whitespaces = tokenize xs
                  | x `elem` digits \\ ['.'] = doDigit [x] xs
                  | x `elem` identifiers = doIdentifier [x] xs
                  | x `elem` operators = doOperator [x] xs
                  | otherwise = EOF : tokenize xs
                  where
                    doDigit stack (d:ds)
                      = if d `elem` digits
                          then
                            doDigit (stack ++ [d]) ds
                          else
                            (if '.' `elem` stack then CONSTF (read stack :: Float) else CONST (read stack :: Int) ) : tokenize (d:ds)
                    
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
                                  "not"    -> NOT
                                  "either" -> ISEITHER
                                  "neither" -> ISNEITHER ) : tokenize (d:ds)
                    
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
                              "..." -> YADAYADA
                              "(" -> OPENPAREN
                              ")" -> CLOSEPAREN
                              "[" -> OPENLIST
                              "]" -> CLOSELIST
                              "()" -> VOIDARGUMENTS) : tokenize (d:ds)

tokenRevision :: Tokens -> Tokens
tokenRevision [] = []
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
  = applyTwice tokenRevision . tokenize

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
        Left $ "Something got wrong: " ++ show exprest


parseFactors :: Tokens -> (Ast, Tokens)
parseFactors (DECLARE:(IDENT x):rest)
  = (Decl x, rest)

-- Parse idents
parseFactors ((IDENT x):rest)
  = (Ident x, rest)

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

parseFactors ((OPENPAREN):(CLOSEPAREN):rest)
  = (Special NuT, rest)

parseFactors (OPENPAREN:MINUS:rest)
  = let
      factor = OPENPAREN : rest
      (parsed, rest2) = parseFactors factor

    in
      (Negate parsed, rest2)

-- Parse parentheses
parseFactors ((OPENPAREN):rest)
  = let
      -- [Token], [Token]
      (paren, restParen) = getRightParentheses rest

      getRightParentheses xs
        = let
            check (n, acc, (y:[]))
              = case y of
                  OPENPAREN ->
                    (n + 1, acc ++ [y], [EOF]) -- ++ [y]

                  CLOSEPAREN ->
                    if n == 0 then
                      (0, acc ++ [y], []) -- ++ [y]
                    else
                      (n - 1, acc ++ [y], [CLOSEPAREN])

                  othertoken ->
                    (n, acc ++ [y], [othertoken])

            check (n, acc, (y:ys))
              = case y of
                  OPENPAREN ->
                    check (n + 1, acc ++ [y], ys) -- ++ [y]

                  CLOSEPAREN ->
                    if n == 0 then
                      (0, acc ++ [y], ys) -- ++ [y]
                      else
                        check (n - 1, acc ++ [y], ys)

                  othertoken ->
                    check (n, acc ++ [y], ys)

            (_, parenCheck, restCheck) = check (0, [], xs)

          in
            (parenCheck, restCheck)
    in
      let
        -- get the head of rest of tokens and checks if there's a comma (below)
        (parsedExpression, restTokens) = parseHighExp paren
      in
        case head restTokens of
          COMMA ->
            (Tuple (parseSeparators paren), restParen)

          LISTPATTERNMATCHING ->
            let
              factors = parseSeparators paren
              heads = init factors
              rtail = last factors

            in
              (ListPM heads rtail, restParen)

          other ->
            astRevision (Parens parsedExpression, restParen)

parseFactors ((OPENLIST):(CLOSELIST):rest)
  = (Special NuL, rest)

parseFactors ((OPENLIST):rest)
  = let
      (bracket, restBracket) = getRightList rest

      getRightList xs
        = let
            check (n, acc, (y:[]))
              = case y of
                  OPENLIST ->
                    (n + 1, acc, [EOF])

                  CLOSELIST ->
                    if n == 0 then
                      (0, acc ++ [y], [])
                    else
                      (n - 1, acc ++ [y], [CLOSELIST])

                  othertoken ->
                    (n, acc ++ [y], [othertoken])

            check (n, acc, (y:ys))
              = case y of
                  OPENLIST ->
                    check (n + 1, acc ++ [y], ys) -- ++ [y]

                  CLOSELIST ->
                    if n == 0 then
                      (0, acc ++ [y], ys) -- ++ [y]
                      else
                        check (n - 1, acc ++ [y], ys)

                  othertoken ->
                    check (n, acc ++ [y], ys)

            (_, listCheck, restCheck) = check (0, [], xs)

          in
            (listCheck, restCheck)
    in
      astRevision (ComprehensionList (parseSeparators $ bracket), restBracket)

-- IF Expression
--
parseFactors ((IF):rest)
  = let
      (stat, restStat) = parseHighExp rest
      (Then thenStat, restThen) = parseHighExp restStat
      (Else elseStat, restElse) = parseHighExp restThen
    
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

parseFactors (_:rest)
  = (Special Undefined, rest)

parseFactor :: Token -> Ast
parseFactor token
  = fst $ parseFactors [token]


-- Parse expressions separated by commas
parseSeparators :: Tokens -> Asts
parseSeparators [] = []
parseSeparators t
  = let
      tuple@(content, tokens) = parseHighExp t

      checkExpression (content, COUNTLIST:rest)
        = [CountList content (fst $ parseHighExp rest)]

      checkExpression (content, COMMA:rest)
        = content : parseSeparators rest

      checkExpression (content, [])
        = [content]

    in
      if null (checkComma tokens)
        then
          [content]
        else
          checkExpression tuple

parseAllFactors :: Tokens -> (Asts, Tokens)
parseAllFactors tokens'
  = let
      parsef' (ast, tokens)
        = let
            (nast, rest) = parseHighExp tokens

            nextToken = if null rest then VOIDENT else head rest
          in
            if (nextToken /= VOIDENT) && not (nextToken `elem` eofers)
              then
                parsef' (ast ++ [nast], rest)

              else
                (ast ++ [nast], rest)
     in
      parsef' ([], tokens')

checkComma :: Tokens -> Tokens
checkComma tokens
  = (if (not $ null tokens) && (head tokens == COMMA) then tail else id) tokens

parseAllFactorsCommas :: Tokens -> (Asts, Tokens)
parseAllFactorsCommas tokens'
  = let
      parsef' (ast, tokens)
        = let
            (nast, rest) = parseHighExp tokens
            eofers'wc = filter (/=COMMA) eofers

          in
            if (null rest) || (head rest) `elem` eofers'wc
              then
                (ast ++ [nast], rest)

              else
                parsef' (ast ++ [nast], tail rest)
     in
      parsef' ([], tokens')

parseEachFactor :: Tokens -> (Asts, Tokens)
parseEachFactor tokens'
  = let
      parsef' (ast, tokens)
        = let
            (nast, rest) = parseFactors tokens

            nextToken = if null rest then VOIDENT else head rest
          in
            if (nextToken /= VOIDENT) && not (nextToken `elem` eofers)
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
          (bodyFunction, rest2) = parseHighExp . tail . dropWhile (/=RARROW) $ restTokens -- Remove Assign from head (tail)

        in
          (LambdaDef arguments bodyFunction, rest2)

      DO ->
        let
          (declarations, IN:rest) = parseAllFactors restTokens
          (todo, rest2) = parseHighExp rest
        
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
            ( Assign factortree subexptree, checkComma rest3 )

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

        (WITH : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Call factortree [subexptree], checkComma rest3 )

        (CALLARGS : rest2) ->
          let
            (subexptree, rest3) = parseAllFactors rest2
          in
            ( Call factortree subexptree, checkComma rest3 )

        (TAKE : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Take factortree subexptree, rest3 )

        (EXP : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Expo factortree subexptree, rest3 )

        (CONCATLIST : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Concat factortree subexptree, rest3 )

        (OPCALLFUNCTION n : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Call (Ident n) [factortree, subexptree], checkComma rest3 )

        (ISEITHER : rest2) ->
          let
            (subexptree, rest3) = parseEachFactor rest2

          in
            ( IsEither factortree subexptree, rest3 )

        (ISNEITHER : rest2) ->
          let
            (subexptree, rest3) = parseEachFactor rest2

          in
            ( IsNeither factortree subexptree, rest3 )

        (LARROW : rest2) ->
          let
            (subexptree, rest3) = parseFactors rest2

          in
            (Call factortree [Call subexptree []], rest3)

        -- Like an 'otherwise'
        othertokens ->   -- TODO
          (factortree, othertokens)


astRevision (Parens (Parens ast), tokens) = (Parens ast, tokens)
astRevision (ComprehensionList [CountList pa pb], tokens) = (CountList pa pb, tokens)

astRevision (Mul ta (Add pa pb), tokens) = (Add (Mul ta pa) pb, tokens)
astRevision (Mul ta (Sub pa pb), tokens) = (Sub (Mul ta pa) pb, tokens)
astRevision (Mul ta (Div pa pb), tokens) = (Div (Mul ta pa) pb, tokens)
astRevision (Mul ta (Grt pa pb), tokens) = (Grt (Mul ta pa) pb, tokens)
astRevision (Mul ta (Ge  pa pb), tokens) = (Ge  (Mul ta pa) pb, tokens)
astRevision (Mul ta (Let pa pb), tokens) = (Let (Mul ta pa) pb, tokens)
astRevision (Mul ta (Le  pa pb), tokens) = (Le  (Mul ta pa) pb, tokens)
astRevision (Mul ta (Equ pa pb), tokens) = (Equ (Mul ta pa) pb, tokens)
astRevision (Mul ta (Not pa pb), tokens) = (Not (Mul ta pa) pb, tokens)
astRevision (Mul ta (Mod pa pb), tokens) = (Mod (Mul ta pa) pb, tokens)
astRevision (Mul ta (Concat pa pb), tokens) = (Concat (Mul ta pa) pb, tokens)

astRevision (Div ta (Add pa pb), tokens) = (Add (Div ta pa) pb, tokens)
astRevision (Div ta (Sub pa pb), tokens) = (Sub (Div ta pa) pb, tokens)
astRevision (Div ta (Grt pa pb), tokens) = (Grt (Div ta pa) pb, tokens)
astRevision (Div ta (Ge  pa pb), tokens) = (Ge  (Div ta pa) pb, tokens)
astRevision (Div ta (Let pa pb), tokens) = (Let (Div ta pa) pb, tokens)
astRevision (Div ta (Le  pa pb), tokens) = (Le  (Div ta pa) pb, tokens)
astRevision (Div ta (Equ pa pb), tokens) = (Equ (Div ta pa) pb, tokens)
astRevision (Div ta (Not pa pb), tokens) = (Not (Div ta pa) pb, tokens)
astRevision (Div ta (Mod pa pb), tokens) = (Mod (Div ta pa) pb, tokens)
astRevision (Div ta (Expo pa pb), tokens) = (Expo (Div ta pa) pb, tokens)
astRevision (Div ta (Concat pa pb), tokens) = (Concat (Div ta pa) pb, tokens)

astRevision (Expo ta (Add pa pb), tokens) = (Add (Expo ta pa) pb, tokens)
astRevision (Expo ta (Sub pa pb), tokens) = (Sub (Expo ta pa) pb, tokens)
astRevision (Expo ta (Mul pa pb), tokens) = (Mul (Expo pa ta) pb, tokens)
astRevision (Expo ta (Div pa pb), tokens) = (Div (Expo ta pa) pb, tokens)
astRevision (Expo ta (Grt pa pb), tokens) = (Grt (Expo ta pa) pb, tokens)
astRevision (Expo ta (Ge  pa pb), tokens) = (Ge  (Expo ta pa) pb, tokens)
astRevision (Expo ta (Let pa pb), tokens) = (Let (Expo ta pa) pb, tokens)
astRevision (Expo ta (Le  pa pb), tokens) = (Le  (Expo ta pa) pb, tokens)
astRevision (Expo ta (Equ pa pb), tokens) = (Equ (Expo ta pa) pb, tokens)
astRevision (Expo ta (Not pa pb), tokens) = (Not (Expo ta pa) pb, tokens)
astRevision (Expo ta (Mod pa pb), tokens) = (Mod (Expo ta pa) pb, tokens)
astRevision (Expo ta (Concat pa pb), tokens) = (Concat (Expo ta pa) pb, tokens)

astRevision pair = pair


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