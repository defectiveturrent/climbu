{-

-}

module Parser where

import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils
import Expressions

parseTokens :: String -> Tokens
parseTokens
  = tokenRevision . tokenise

parseEofs :: Tokens -> Asts
parseEofs [] = []
parseEofs tokens
  = let
      (pretokens, rest) = break (==EOF) tokens
      (exptree, exprest) = parseHighExp pretokens

    in
      if null exprest then
        if null rest then
          exptree : []
        else
          exptree : (parseEofs $ tail rest)

      else
        error $ "\nWhoops, something wrong here:\n\t" ++ show exprest

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
tokenise ('-':'>':rest)     = ARROW : tokenise rest
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
  | not $ isName x =  WITH : tokenise (x:rest)

tokenise ('i':'f':x:rest)
  | not $ isName x =  IF : tokenise (x:rest)

tokenise ('t':'h':'e':'n':x:rest)
  | not $ isName x =  THEN : tokenise (x:rest)

tokenise ('e':'l':'s':'e':x:rest)
  | not $ isName x =  ELSE : tokenise (x:rest)

tokenise ('l':'e':'t':x:rest)
  | not $ isName x = LET : tokenise (x:rest)

tokenise ('f':'o':'r':x:rest)
  | not $ isName x =  FOR : tokenise (x:rest)

tokenise ('i':'s':x:'e':'i':'t':'h':'e':'r':y:rest)
  | not $ isName x
  , not $ isName y
    = ISEITHER : tokenise (y:rest)

tokenise ('i':'s':x:'n':'e':'i':'t':'h':'e':'r':y:rest)
  | not $ isName x
  , not $ isName y
    = ISNEITHER : tokenise (y:rest)

tokenise ('i':'n':x:rest)
  | not $ isName x =  IN : tokenise (x:rest)

tokenise ('w':'h':'e':'n':x:rest)
  | not $ isName x =  WHEN : tokenise (x:rest)

tokenise ('d':'e':'f':x:rest)
  | not $ isName x =  FUNC : tokenise (x:rest)

tokenise ('l':'a':'m':x:rest)
  | not $ isName x =  LAMBDA : tokenise (x:rest)

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


getname :: [Char] -> ([Char], [Char]) -- (name, rest)
getname str
  = let
      getname' [] chs = (chs, [])
      getname' (ch : str) chs
        | isName ch = getname' str (chs++[ch])
        | otherwise  = (chs, ch : str)
    in
      getname' str []

isName ch
  = isAlpha ch || ch == '_'

convert :: [Char] -> (Int, [Char])
convert str
  = let
      conv' [] n = (n, [])
      conv' (ch : str) n
        | isDigit ch = conv' str ((n*10) + (read [ch] :: Int))
        | otherwise  = (n, ch : str)
    in 
      conv' str 0

tokenRevision :: [Token] -> [Token]
tokenRevision [] = []
tokenRevision (FUNC:rest)
  = let
      (body, rest2) = break (==ASSIGN) rest
    in
      FUNC : body ++ tokenRevision rest2

tokenRevision (LAMBDA:rest)
  = let
      (body, rest2) = break (==ARROW) rest
    in
      LAMBDA : body ++ tokenRevision rest2

tokenRevision ((ID x):rest)
  = let
      (arguments, rest2) = getUntilEofer ([], rest)

      getUntilEofer (acc, [])
        = (acc, [])
        
      getUntilEofer (acc, x:xs) | x `elem` eofers = (acc, x:xs)
                                | otherwise = getUntilEofer (acc ++ [x], xs)

    in
      if null arguments
        then
          (ID x) : tokenRevision rest
        else
          [ID x] ++ [CALLARGS] ++ arguments ++ tokenRevision rest2

tokenRevision (x:rest) = x : tokenRevision rest


parser :: [Token] -> Ast
parser tokens
  = let
      (tree, rest) = parseHighExp tokens -- (Ast, [Token])
    in
      if null rest then
        tree
      else
        error $ "\nWhoops, something wrong here:\n\t" ++ show rest

parseFactor :: Token -> Ast
parseFactors :: [Token] -> (Ast, [Token])

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
              (Parens parsedExpression, restParen)

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
        (ComprehensionList (parseSeparators $ bracket), restBracket)

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

parseFactor token
    = fst $ parseFactors [token]


-- Parse expressions separated by commas
parseSeparators [] = []
parseSeparators pair
  = let
      (content, rest) = parseHighExp pair

    in
      if null rest
        then
          [content]
        else
          content : (parseSeparators $ tail rest)

parseAllFactors :: [Token] -> ([Ast], [Token])
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

checkComma :: [Token] -> [Token]
checkComma tokens
  = (if (not $ null tokens) && (head tokens == COMMA) then tail else id) tokens

parseAllFactorsCommas :: [Token] -> ([Ast], [Token])
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


parseHighExp :: [Token] -> (Ast, [Token])
parseHighExp []
  = (Eof, [])

parseHighExp tokens@( _ : [] )
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
          arguments = fst . parseAllFactors . takeWhile (/=ARROW) $ restTokens
          (bodyFunction, rest2) = parseHighExp . tail . dropWhile (/=ARROW) $ restTokens -- Remove Assign from head (tail)

        in
          (LambdaDef arguments bodyFunction, rest2)

      LET ->
        let
          (declarations, IN:rest) = parseAllFactors restTokens
          (todo, rest2) = parseHighExp rest
        
        in
          (LetIn declarations todo, rest2)

      othertokens ->
        parseExp tokens

parseExp :: [Token] -> (Ast, [Token])
parseExp [] = (Eof, [])
parseExp tokens
  = let
      (factortree, rest) = parseFactors tokens

    in
      case rest of
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
            ( Add factortree subexptree, rest3 )

        (MINUS : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Sub factortree subexptree, rest3 )

        (MUL : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Mul factortree subexptree, rest3 )

        (DIV : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( (case subexptree of
                Num 0 ->
                  Special Infinite

                _ ->
                  Div factortree subexptree
            ), checkComma rest3 )

        (GREATERTHAN : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Grt factortree subexptree, rest3 )

        (GREATEREQUAL : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Ge factortree subexptree, rest3 )

        (LESSTHAN : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Let factortree subexptree, rest3 )

        (LESSEQUAL : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Le factortree subexptree, rest3 )

        (EQUAL : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Equ factortree subexptree, rest3 )
        
        (NOT : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Not factortree subexptree, rest3 )

        (MOD : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Mod factortree subexptree, rest3 )

        (WITH : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( Call factortree [subexptree], checkComma rest3 )

        (COUNTLIST : rest2) ->
          let
            (subexptree, rest3) = parseHighExp rest2
          in
            ( CountList factortree subexptree, rest3)

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
            ( Expo factortree subexptree, rest3 )

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
            (subexptree, rest3) = parseAllFactors rest2

          in
            ( IsEither factortree subexptree, rest3 )

        (ISNEITHER : rest2) ->
          let
            (subexptree, rest3) = parseAllFactors rest2

          in
            ( IsNeither factortree subexptree, rest3 )

        -- Like an 'otherwise'
        othertokens ->   -- TODO
          (factortree, othertokens)


