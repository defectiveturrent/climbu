module Translator where

import Data.Char
import Data.List
import Data.Maybe
import Expressions
import Parser

-- Instructions
--
data Inst
  = PushVar String
  | PushConst String
  | PushChar Char
  | AssignTo Inst Inst
  | Operation String Inst Inst
  | ForList Inst Inst Inst Inst   -- Var; Result; Ranges; Condition.
  | MakeCountList Inst Inst
  | MakeSimpleList [Inst]
  | Block Inst
  | MakeCondition Inst Inst Inst
  | Function Inst [Inst] Inst     -- auto foo = [](auto a, auto b){ return a + b; }
  | Lambda [Inst] Inst            -- [](auto a, auto b){ }
  | CallFunction Inst [Inst]      -- foo (1, x, "Hey")
  | DoTake Inst Inst              -- list take n
  | ConcatList Inst Inst          -- [1, 2] ++ [3, 4]
  | TNothing
  | Error String                  -- For format errors
  deriving (Show, Read, Eq)


parseAst (Void) = TNothing
parseAst (Ident x) = PushVar x
parseAst (CharString x) = MakeSimpleList $ map PushChar x
parseAst (CharByte x) = PushChar x
parseAst (Num x) = PushConst (show x)
parseAst (Assign e1 e2) = AssignTo (parseAst e1) (parseAst e2)
parseAst (Take e1 e2) = DoTake (parseAst e1) (parseAst e2)
parseAst (Expo e1 e2) = CallFunction (PushVar "pow") [parseAst e1, parseAst e2]
parseAst (Concat e1 e2) = ConcatList (parseAst e1) (parseAst e2)

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

parseAst (For e1 e2 e3)
  = let
      (In var range) = e2
      (When condition) = e3

      var' = parseAst var
      result' = parseAst e1
      range' = parseAst range
      condition' = case condition of
                    Void ->
                      PushConst "true"

                    other ->
                      parseAst condition

    in
      ForList var' (Lambda [var'] result') range' (Lambda [var'] condition')

parseAst (ComprehensionList [CountList e1 e2]) = MakeCountList (parseAst e1) (parseAst e2)

-- Countlist error
parseAst (ComprehensionList ((CountList e1 e2):rest))
  = Error $
      "\nHey! Lists cannot has two or more countlists:\n\t["
      ++ (show $ parseAst e1)
      ++ ".."
      ++ (show $ parseAst e2)
      ++ ", ..]\n"

parseAst (ComprehensionList xs) = MakeSimpleList (map parseAst xs)
parseAst (Parens e1) = Block (parseAst e1)
parseAst (Def name args body) = Function (parseAst name) (map parseAst args) (parseAst body)
parseAst (LambdaDef args body) = Lambda (map parseAst args) (parseAst body)
parseAst (Call name args) = CallFunction (parseAst name) (map parseAst args)
parseAst (Condition stat thenStat elseStat) = Block $ MakeCondition (parseAst stat) (parseAst thenStat) (parseAst elseStat)
parseAst (IsEither e1 e2) = CallFunction (PushVar "elem") [parseAst e1, MakeSimpleList $ map parseAst e2]
parseAst (IsNeither e1 e2) = Block $ CallFunction (PushVar "!elem") [parseAst e1, MakeSimpleList $ map parseAst e2]
parseAst _ = TNothing


-- To translate tokens to instructions, you should do this:
--
--   execute $ tokenise "def foo a = a * 2 and print with foo in 5"
--

execute :: [Token] -> [String]
execute stack
  = let
      paragraphs :: [Ast]
      paragraphs = parseEofs stack

      outcode = map parseAst paragraphs

    in
      map (\x -> (translate x) ++ ";") outcode

genCode :: String -> String
genCode stack
  = "#include \"standard.hpp\"\n\n"
  ++ (intercalate "\n" . execute . tokenise $ stack)
  ++ "\n\n"
  ++ "int main( int countArgs, char** args )\n"
  ++ "{\n"
  ++ "  return _main();\n"
  ++ "}"

translate :: Inst -> String
translate inst
  = case inst of
      PushVar x ->
        x

      PushConst x ->
        x

      PushChar x ->
        if x == '\''
          then
            "'\\''"
          else
            ['\'', x, '\'']

      AssignTo i1 i2 ->
        "auto " ++ (translate i1) ++ " = " ++ (translate i2)

      Operation op i1 i2 ->
        (translate i1) ++ op ++ (translate i2)

      ForList var fresult range fcondition ->
        "eachlist(" ++ (translate fresult) ++ ", " ++ (translate range) ++ ", " ++ (translate fcondition) ++ ")"

      MakeCountList min_ max_ ->
        "countlist(" ++ (translate min_) ++ ", " ++ (translate max_) ++ ")"

      MakeSimpleList content ->
        "vector<decltype(" ++ (translate (head content)) ++ ")>({" ++ (intercalate "," $ map translate content) ++ "})"

      Block i ->
        "(" ++ (translate i) ++ ")"

      --"if(" ++ (translate statif) ++ "){" ++ (translate statthen) ++ ";}else{" ++ (translate statelse) ++ ";};"
      MakeCondition statif statthen statelse ->
        (translate statif) ++ "?" ++ (translate statthen) ++ ":" ++ (translate statelse)

      Function name args body ->
        let
          strname = translate name
          checkName = if strname == "main" then "_main" else strname

          --line = translate $ AssignTo (PushVar checkName) (Lambda args body)
          line
            =  genGenericPrefix (if args /= [TNothing] then length args else 0)
            ++ checkName
            ++ genGenericArguments args
            ++ "\n{ return "
            ++ (translate body)
            ++ "; }"
        in
          line

      Lambda args body ->
        "[&](" ++ (intercalate "," $ map (\x -> "auto "++(translate x)) args) ++ "){ return " ++ (translate body) ++ "; }"

      CallFunction name args ->
        (translate name) ++ "(" ++ (intercalate "," $ map translate args) ++ ")"

      DoTake i1 i2 ->
        (translate i1) ++ "[" ++ (translate i2) ++ "]"

      ConcatList i1 i2 ->
        "conc(" ++ (translate i1)  ++ "," ++ (translate i2) ++ ")"

      Error msg ->
        error msg


genGenericPrefix n
  = let
      prefix = "template<"
      suffix = "> auto "
      classes = map (\number -> "class t_" ++ show number) [1..n]

      completePrefix
        =  prefix
        ++ (intercalate ", " classes)
        ++ suffix

    in
      if n /= 0
        then
          completePrefix
        else
          "auto "

genGenericArguments args
  = let
      prefix = "("
      suffix = ")"
      classes
        = if args /= [TNothing]
            then
              map (\number -> "t_" ++ show number) [1 .. length args]
            else
              []

      completeArguments
        =  prefix
        ++ intercalate ", " (map (\(a, b) -> unwords [a, b]) $ zip classes (map translate args))
        ++ suffix

    in
      completeArguments




