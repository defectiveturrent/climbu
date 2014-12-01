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

tokenise :: String -> Tokens
tokenise [] = []                    -- (end of input)
tokenise (' ':rest) = tokenise rest      -- (skip spaces)
tokenise ('\n':rest) = tokenise rest
tokenise ('\r':rest) = tokenise rest
tokenise ('\t':rest) = tokenise rest

tokenise ('"':'"':rest) = NULLSTRING : tokenise rest

tokenise ('"':rest)
  = let
      subtokenise ('\\' : '"' : rest2) acc
        = subtokenise (rest2) (acc ++ "\"")

      subtokenise ('"' : rest2) acc
        = STRING acc : tokenise rest2

      subtokenise (x : rest2) acc
        = subtokenise (rest2) (acc ++ [x])

    in
      subtokenise rest []


tokenise ('/':'/':rest)             -- (comments)
  = let
      (_, rest2) = break (=='\n') rest

    in
      tokenise (if null rest2 then [] else tail rest2)

tokenise ('(':'*':rest)             -- (comments)
  = let
      subtokenise [] = []
      subtokenise ('*':')':rest2)
        = tokenise rest2

      subtokenise (_:rest2)
        = subtokenise rest2

    in
      subtokenise rest

tokenise (';':rest)         = EOF : tokenise rest
tokenise ('.':'.':'.':rest) = YADAYADA : tokenise rest
tokenise ('<':'-':rest)     = LARROW : tokenise rest
tokenise ('-':'>':rest)     = RARROW : tokenise rest
tokenise ('(':rest)         = OPENPAREN : tokenise rest
tokenise (')':rest)         = CLOSEPAREN : tokenise rest
tokenise ('[':rest)         = OPENBRACKETS : tokenise rest
tokenise (']':rest)         = CLOSEBRACKETS : tokenise rest
tokenise ('{':rest)         = OPENKEYS : tokenise rest
tokenise ('}':rest)         = CLOSEKEYS : tokenise rest
tokenise ('\'':x:'\'':rest) = CHAR x : tokenise rest
tokenise ('.':'.':rest)     = COUNTLIST : tokenise rest
tokenise ('.':rest)         = CALLARGS : tokenise rest
tokenise ('+':'+':rest)     = CONCATLIST : tokenise rest
tokenise ('+':rest)         = PLUS : tokenise rest
tokenise ('-':rest)         = MINUS : tokenise rest
tokenise ('*':rest)         = MUL : tokenise rest
tokenise ('/':'=':rest)     = NOT : tokenise rest
tokenise ('/':rest)         = DIV : tokenise rest
tokenise ('%':rest)         = MOD : tokenise rest
tokenise ('^':rest)         = EXPO : tokenise rest
tokenise ('|':'>':rest)     = TAKE : tokenise rest
tokenise ('>':'=':rest)     = GREATEREQUAL : tokenise rest
tokenise ('>':rest)         = GREATERTHAN : tokenise rest
tokenise ('<':'=':rest)     = LESSEQUAL : tokenise rest
tokenise ('<':rest)         = LESSTHAN : tokenise rest
tokenise ('=':'=':rest)     = EQUAL : tokenise rest
tokenise ('=':rest)         = ASSIGN : tokenise rest
tokenise (',':rest)         = COMMA : tokenise rest
tokenise ('|':rest)         = ELSE : IF : tokenise rest
tokenise (':':rest)         = LISTPATTERNMATCHING : tokenise rest

tokenise ('`':rest)
  = let
      (n, '`':rest2) = getname rest

    in
      OPCALLFUNC n : tokenise rest2

tokenise ('n':'u':'l':'l':[]) = [NULL]

tokenise ('n':'u':'l':'l':x:rest)
  | not $ isName x
  = NULL : tokenise (x:rest)

tokenise ('c':'a':'l':'l':x:rest)
  | not $ isName x
  , isName $ head rest
  = let
      (n, rest2) = getname rest

    in 
      CALLALONE n : tokenise rest2

tokenise ('i':'m':'p':'o':'r':'t':x:rest)
  | not $ isName x
  = let
      (path, rest2) = break (==';') rest
    in
      IMPORT path : tokenise rest2

tokenise ('w':'h':'i':'l':'e':x:rest)
  | not $ isName x = WHILE : tokenise (x:rest)

tokenise ('w':'i':'t':'h':x:rest)
  | not $ isName x = WITH : tokenise (x:rest)

tokenise ('i':'f':x:rest)
  | not $ isName x = IF : tokenise (x:rest)

tokenise ('t':'h':'e':'n':x:rest)
  | not $ isName x = THEN : tokenise (x:rest)

tokenise ('e':'l':'s':'e':x:rest)
  | not $ isName x = ELSE : tokenise (x:rest)

tokenise ('d':'o':x:rest)
  | not $ isName x = DO : tokenise (x:rest)

tokenise ('f':'o':'r':x:rest)
  | not $ isName x = FOR : tokenise (x:rest)

tokenise ('v':'a':'r':x:rest)
  | not $ isName x = VAR : tokenise (x:rest)

tokenise ('i':'s':x:'e':'i':'t':'h':'e':'r':y:rest)
  | not $ isName x
  , not $ isName y
    = ISEITHER : tokenise (y:rest)

tokenise ('i':'s':x:'n':'e':'i':'t':'h':'e':'r':y:rest)
  | not $ isName x
  , not $ isName y
    = ISNEITHER : tokenise (y:rest)

tokenise ('i':'n':x:rest)
  | not $ isName x = IN : tokenise (x:rest)

tokenise ('w':'h':'e':'n':x:rest)
  | not $ isName x = WHEN : tokenise (x:rest)

tokenise ('d':'e':'f':x:rest)
  | not $ isName x = FUNC : tokenise (x:rest)

tokenise ('l':'a':'m':x:rest)
  | not $ isName x = LAMBDA : tokenise (x:rest)

tokenise (ch:rest)
  | isDigit ch
    = let
        (n, rest2) = convert (ch:rest)
      in 
        (CONST n):(tokenise rest2)

tokenise (ch:rest)
  | isName ch
    = let
        (n, rest2) = getname (ch:rest)
      in 
        (ID n):(tokenise rest2)


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

isName ch
  = isAlpha ch || ch == '_'

isDigitName ch
  = isName ch || isDigit ch

convert :: String -> (Int, String)
convert str
  = let
      conv' [] n = (n, [])
      conv' (ch : str) n
        | isDigit ch = conv' str ((n*10) + (read [ch] :: Int))
        | otherwise  = (n, ch : str)
    in 
      conv' str 0

tokenRevision :: Tokens -> Tokens
tokenRevision [] = []
tokenRevision (ID x:OPCALLFUNC n:rest)
  = ID x : OPCALLFUNC n : tokenRevision rest

tokenRevision (FUNC:rest)
  = let
      (body, rest2) = break (==ASSIGN) rest
    in
      FUNC : body ++ tokenRevision rest2

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

tokenRevision (CLOSEPAREN:ID x:rest)
  = CLOSEPAREN : MUL : ID x : tokenRevision rest

tokenRevision (CLOSEPAREN:CONST x:rest)
  = CLOSEPAREN : MUL : CONST x : tokenRevision rest

tokenRevision (CONST x:OPENPAREN:rest)
  = CONST x : MUL : OPENPAREN : tokenRevision rest

tokenRevision (CONST x:ID y:rest)
  = CONST x : MUL : ID y : tokenRevision rest

--tokenRevision ((ID x):OPENPAREN:rest)
--  = ID x : CALLARGS : OPENPAREN : tokenRevision rest

tokenRevision (ID x:OPENBRACKETS:rest)
  = ID x : CALLARGS : OPENBRACKETS : tokenRevision rest

tokenRevision (ID x:OPENPAREN:rest)
  = formuled
  where
    isThereSomethingWrong
      = foldr
        (\x acc -> case x of
             ID _ -> acc
             _ -> True )
        False

    formuled = if isThereSomethingWrong args
                then
                  ID x : CALLARGS : OPENPAREN : tokenRevision rest
                else
                  tokenRevision (FUNC : ID x : (args ++ as))

    (args, CLOSEPAREN:as) = break (==CLOSEPAREN) rest

tokenRevision ((ID x):rest)
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
          (ID x) : tokenRevision rest
        else
          [OPENPAREN, ID x, CALLARGS] ++ arguments ++ [CLOSEPAREN] ++ tokenRevision rest2

tokenRevision (STRING a : STRING b : rest)
  = STRING a : CONCATLIST : tokenRevision (STRING b : rest)

tokenRevision (x:rest) = x : tokenRevision rest

parseTokens :: String -> Tokens
parseTokens
  = tokenRevision . tokenise

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
parseFactors (VAR:(ID x):rest)
  = (Decl x, rest)

-- Parse idents
parseFactors ((ID x):rest)
  = (Ident x, rest)

parseFactors ((STRING str):rest)
  = (CharString str, rest)

parseFactors ((CHAR ch):rest)
  = (CharByte ch, rest)

-- Parse numbers
parseFactors ((CONST x):rest)
  = (Num x, rest)

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

parseFactors ((OPENBRACKETS):(CLOSEBRACKETS):rest)
  = (Special NuL, rest)

parseFactors ((OPENBRACKETS):rest)
  = let
      (bracket, restBracket) = getRightList rest

      getRightList xs
        = let
            check (n, acc, (y:[]))
              = case y of
                  OPENBRACKETS ->
                    (n + 1, acc, [EOF])

                  CLOSEBRACKETS ->
                    if n == 0 then
                      (0, acc ++ [y], [])
                    else
                      (n - 1, acc ++ [y], [CLOSEBRACKETS])

                  othertoken ->
                    (n, acc ++ [y], [othertoken])

            check (n, acc, (y:ys))
              = case y of
                  OPENBRACKETS ->
                    check (n + 1, acc ++ [y], ys) -- ++ [y]

                  CLOSEBRACKETS ->
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
      FUNC ->
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

        (GREATERTHAN : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Grt factortree subexptree, rest3 )

        (GREATEREQUAL : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Ge factortree subexptree, rest3 )

        (LESSTHAN : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Let factortree subexptree, rest3 )

        (LESSEQUAL : rest2) ->
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

        {-(COUNTLIST : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( CountList factortree subexptree, rest3) -}

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

        (EXPO : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            astRevision ( Expo factortree subexptree, rest3 )

        (CONCATLIST : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Concat factortree subexptree, rest3 )

        (OPCALLFUNC n : rest2) ->
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
astRevision (Expo ta (Mul pa pb), tokens) = (Mul (Expo ta pa) pb, tokens)
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