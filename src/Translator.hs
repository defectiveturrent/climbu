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
  | PushString String
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
  | TNothing
  | Error String                  -- For format errors
  deriving (Show, Read, Eq)


parseAst (Void) = TNothing
parseAst (Ident x) = PushVar x
parseAst (CharString x) = PushString x
parseAst (Num x) = PushConst (show x)
parseAst (Assign e1 e2) = AssignTo (parseAst e1) (parseAst e2)
parseAst (Take e1 e2) = DoTake (parseAst e1) (parseAst e2)

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
parseAst (ParenthesesBlock e1) = Block (parseAst e1)
parseAst (Def name args body) = Function (parseAst name) (map parseAst args) (parseAst body)
parseAst (LambdaDef args body) = Lambda (map parseAst args) (parseAst body)
parseAst (Call name args) = CallFunction (parseAst name) (map parseAst args)
parseAst (Condition stat thenStat elseStat) = MakeCondition (parseAst stat) (parseAst thenStat) (parseAst elseStat)


-- To translate tokens to instructions, you should do this:
--
-- execute $ tokenise "def foo a = a * 2 and print with foo in 5"

execute :: [Token] -> [String]
execute stack
  = let
      paragraphs :: [Ast]
      paragraphs = parseEofs stack

      outcode = map parseAst paragraphs

    in
      map (\x -> (translate x) ++ ";") outcode

genCode stack
  = "#include \"standard.hpp\"\n\n"
  ++ "int main( int countArgs, char** args )\n"
  ++ "{\n"
  ++ (intercalate "\n" . execute . tokenise $ stack) ++ "\n"
  ++ "return 0;\n"
  ++ "}"

translate :: Inst -> String
translate inst
  = case inst of
      PushVar x ->
        x

      PushConst x ->
        x

      PushString x ->
        "\"" ++ x ++ "\""

      AssignTo i1 i2 ->
        "auto " ++ (translate i1) ++ " = " ++ (translate i2)

      Operation op i1 i2 ->
        (translate i1) ++ op ++ (translate i2)

      ForList var fresult range fcondition ->
        "eachlist(" ++ (translate fresult) ++ ", " ++ (translate range) ++ ", " ++ (translate fcondition) ++ ")"

      MakeCountList min_ max_ ->
        "countlist(" ++ (translate min_) ++ ", " ++ (translate max_) ++ ")"

      MakeSimpleList content ->
        "{" ++ (intercalate "," $ map translate content) ++ "}"

      Block i ->
        "(" ++ (translate i) ++ ")"

      --"if(" ++ (translate statif) ++ "){" ++ (translate statthen) ++ ";}else{" ++ (translate statelse) ++ ";};"
      MakeCondition statif statthen statelse ->
        (translate statif) ++ "?" ++ (translate statthen) ++ ":" ++ (translate statelse)

      Function name args body ->
        let
          strname = translate name
          checkName = if strname == "main" then "_main" else strname

          line = "auto " ++ checkName ++ " = [=](" ++ (intercalate "," $ map (\x -> "auto "++(translate x)) args) ++ "){ return " ++ (translate body) ++ "; }"
        in
          if strname /= "main"
            then
              line

            else
              line ++ ";\n_main()" 

      Lambda args body ->
        "[](" ++ (intercalate "," $ map (\x -> "auto "++(translate x)) args) ++ "){ return " ++ (translate body) ++ "; }"

      CallFunction name args ->
        (translate name) ++ "(" ++ (intercalate "," $ map translate args) ++ ")"

      DoTake i1 i2 ->
        (translate i1) ++ "[" ++ (translate i2) ++ "]"

      Error msg ->
        error msg

