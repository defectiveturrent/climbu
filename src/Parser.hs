module Parser where

import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils
import Expressions

parseEofs :: [Token] -> [Ast]
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
tokenise ('"':rest)
  = let
      (string, _:rest2) = break (=='"') rest

    in
      (STRING string) : tokenise rest2

tokenise ('/':'/':rest)             -- (comments)
  = let
      (_, _:rest2) = break (=='\n') rest

    in
      tokenise rest2

tokenise (' ':rest) = tokenise rest      -- (skip spaces)
tokenise ('(':rest) = OPENPAREN : tokenise rest
tokenise (')':rest) = CLOSEPAREN : tokenise rest
tokenise ('[':rest) = OPENBRACKETS : tokenise rest
tokenise (']':rest) = CLOSEBRACKETS : tokenise rest
tokenise ('{':rest) = OPENKEYS : tokenise rest
tokenise ('}':rest) = CLOSEKEYS : tokenise rest
tokenise ('.':'.':rest) = COUNTLIST : tokenise rest
tokenise ('+':rest) = PLUS : tokenise rest
tokenise ('-':rest) = MINUS : tokenise rest
tokenise ('*':rest) = MUL : tokenise rest
tokenise ('/':rest) = DIV : tokenise rest
tokenise ('%':rest) = MOD : tokenise rest
tokenise ('>':'=':rest) = GREATEREQUAL : tokenise rest
tokenise ('>':rest) = GREATERTHAN : tokenise rest
tokenise ('<':'=':rest) = LESSEQUAL : tokenise rest
tokenise ('<':rest) = LESSTHAN : tokenise rest
tokenise ('=':'=':rest) = EQUAL : tokenise rest
tokenise ('=':rest) = ASSIGN : tokenise rest
tokenise (',':rest) = COMMA : tokenise rest

tokenise ('a':'n':'d':' ':rest)
  = EOF : tokenise rest

tokenise ('e':'a':'c':'h':' ':rest)
  = EACH : tokenise rest

tokenise ('w':'i':'t':'h':' ':rest)
  = WITH : tokenise rest

tokenise ('i':'n':' ':rest)
  = CALLARGS : tokenise rest

tokenise ('i':'f':' ':rest)
  = IF : tokenise rest

tokenise ('t':'h':'e':'n':' ':rest)
  = THEN : tokenise rest

tokenise ('e':'l':'s':'e':' ':rest)
  = ELSE : tokenise rest

tokenise ('w':'h':'i':'l':'e':' ':rest)
  = WHILE : tokenise rest

tokenise ('f':'o':'r':' ':rest)
  = FOR : tokenise rest

tokenise ('d':'e':'f':' ':rest)
  = FUNC : tokenise rest

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

-- Parse numbers
parseFactors ((CONST x):rest)
    = (Num x, rest)

-- Parse parentheses
parseFactors ((OPENPAREN):rest)
    = let
        (paren, restParen) = getRightParentheses rest

        getRightParentheses xs
          = let
              check (n, acc, (y:[]))
                = case y of
                    OPENPAREN ->
                      (n + 1, acc, [EOF]) -- ++ [y]

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
                      check (n + 1, (if n == 0 then acc ++ [y] else acc), ys) -- ++ [y]

                    CLOSEPAREN ->
                      if n == 0 then
                        (0, acc ++ [y], ys) -- ++ [y]
                        else
                          check (n - 1, (if n == 0 then acc ++ [y] else acc), ys)

                    othertoken ->
                      check (n, acc ++ [y], ys)

              (_, parenCheck, restCheck) = check (0, [], xs)

            in
              (parenCheck, restCheck)
      in
        (ParenthesesBlock (fst $ parseHighExp paren), restParen)

parseFactors ((OPENBRACKETS):rest)
    = let
        (bracket, restBracket) = getRightList rest

        separateCommas :: [Token] -> [[Token]]
        separateCommas tokens
          = case dropWhile (==COMMA) tokens of
              [] ->
                []

              other ->
                let
                  (token, restTokens)
                    = break (==COMMA) other
                in
                  token : separateCommas restTokens

        parseIndices
          = map (fst . parseHighExp) . separateCommas

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
                      check (n + 1, (if n == 0 then acc ++ [y] else acc), ys)

                    CLOSEBRACKETS ->
                      if n == 0 then
                        (0, acc ++ [y], ys)
                        else
                          check (n - 1, (if n == 0 then acc ++ [y] else acc), ys)

                    othertoken ->
                      check (n, acc ++ [y], ys)

              (_, listCheck, restCheck) = check (0, [], xs)

            in
              (listCheck, restCheck)
      in
        (ComprehensionList (parseIndices bracket), restBracket)

parseFactors ((OPENKEYS):rest)
    = let
        (key, restKeys) = getRightKeys rest

        getRightKeys xs
          = let
              check (n, acc, (y:[]))
                = case y of
                    OPENKEYS ->
                      (n + 1, acc ++ [y], [EOF])

                    CLOSEKEYS ->
                      if n == 0 then
                        (0, acc ++ [y], [])
                      else
                        (n - 1, acc ++ [y], [CLOSEKEYS])

                    othertoken ->
                      (n, acc ++ [y], [othertoken])

              check (n, acc, (y:ys))
                = case y of
                    OPENKEYS ->
                      check (n + 1, acc ++ [y], ys)

                    CLOSEKEYS ->
                      if n == 0 then
                        (0, acc ++ [y], ys)
                        else
                          check (n - 1, acc ++ [y], ys)

                    othertoken ->
                      check (n, acc ++ [y], ys)

              (_, keyCheck, restCheck) = check (0, [], xs)

            in
              (LAMBDA : keyCheck, restCheck)
      in
        (fst $ parseHighExp key, restKeys)

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

-- Parse end of
parseFactors ((EOF):rest)
    = (Eof, rest)

parseFactor token
    = fst $ parseFactors [token]

parseAllFactors [] = []
parseAllFactors (CLOSEPAREN:_) = []
parseAllFactors (CLOSEBRACKETS:_) = []
parseAllFactors (CLOSEKEYS:_) = []
parseAllFactors (EOF:_) = []
parseAllFactors tokens
  = let
      (ast, rest) = parseFactors tokens

    in
      ast : parseAllFactors rest

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
          arguments = parseAllFactors . takeWhile (/=ASSIGN) $ rest
          (bodyFunction, rest2) = parseHighExp . tail . dropWhile (/=ASSIGN) $ rest -- Remove Assign from head (tail)
            
        in
          (Def functionIdent arguments bodyFunction, rest2)

      LAMBDA ->
        let
          arguments = parseAllFactors . takeWhile (/=ASSIGN) $ restTokens
          (bodyFunction, rest2) = parseHighExp . tail . dropWhile (/=ASSIGN) $ restTokens -- Remove Assign from head (tail)

        in
          (LambdaDef arguments bodyFunction, rest2)

      otherstokens ->
        parseExp tokens

parseExp :: [Token] -> (Ast, [Token])
parseExp tokens
  = let
      (factortree, rest) = parseFactors tokens
    in
      case rest of
      (ASSIGN : rest2) -> 
        let
          (subexptree, rest3) = parseHighExp rest2
        in
          ( Assign factortree subexptree, rest3 )

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
          ( Div factortree subexptree, rest3 )

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

      (MOD : rest2) ->
        let
          (subexptree, rest3) = parseHighExp rest2
        in
          ( Mod factortree subexptree, rest3 )

      (EACH : rest2) ->
        let
          (subexptree, rest3) = parseHighExp rest2
        in
          ( Each factortree subexptree, rest3 )

      (WITH : rest2) ->
        let
          (subexptree, rest3) = parseHighExp rest2
        in
          ( Call factortree [subexptree], rest3 )

      (COUNTLIST : rest2) ->
        let
          (subexptree, rest3) = parseHighExp rest2
        in
          ( CountList factortree subexptree, rest3)

      (CALLARGS : rest2) ->
        let
          (subexptree, rest3) = (parseAllFactors rest2, [])
        in
          ( Call factortree subexptree, rest3)

      -- Like an 'otherwise'
      othertokens -> (factortree, othertokens)


