module Translator where

import Data.Char
import Data.List
import Data.Maybe
import Data.String.Utils
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
  | EachTo Inst Inst
  | MakeCountList Inst Inst
  | MakeSimpleList [Inst]
  | Block Inst
  | MakeCondition Inst Inst Inst
  | Function Inst [Inst] Inst     -- auto foo = [](auto a, auto b){ return a + b; }
  | Lambda [Inst] Inst            -- [](auto a, auto b){ }
  | CallFunction Inst [Inst]      -- foo (1, x, "Hey")
  | Error String                  -- For format errors
  deriving (Show, Read, Eq)


parseAst (Ident x) = PushVar x
parseAst (CharString x) = PushString x
parseAst (Num x) = PushConst (show x)
parseAst (Assign e1 e2) = AssignTo (parseAst e1) (parseAst e2)

parseAst (Add e1 e2) = Operation "+" (parseAst e1) (parseAst e2)
parseAst (Sub e1 e2) = Operation "-" (parseAst e1) (parseAst e2)
parseAst (Mul e1 e2) = Operation "*" (parseAst e1) (parseAst e2)
parseAst (Div e1 e2) = Operation "/" (parseAst e1) (parseAst e2)
parseAst (Mod e1 e2) = Operation "%" (parseAst e1) (parseAst e2)
parseAst (Grt e1 e2) = Operation ">" (parseAst e1) (parseAst e2)
parseAst (Let e1 e2) = Operation "<" (parseAst e1) (parseAst e2)

parseAst (Equ e1 e2) = Operation "==" (parseAst e1) (parseAst e2)
parseAst (Ge  e1 e2) = Operation ">=" (parseAst e1) (parseAst e2)
parseAst (Le  e1 e2) = Operation "<=" (parseAst e1) (parseAst e2)

parseAst (Each e1 e2) = EachTo (parseAst e1) (parseAst e2)
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

type Stack = [Token]

execute :: Stack -> [String]
execute stack
  = let
      paragraphs :: [Ast]
      paragraphs = parseEofs stack

      outcode = map parseAst paragraphs

    in
      map (\x -> (translate x) ++ ";") outcode

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
        (translate i1) ++ " = " ++ (translate i2)

      Operation op i1 i2 ->
        (translate i1) ++ op ++ (translate i2)

      EachTo lambda list ->
        "eachlist(" ++ (translate lambda) ++ ", " ++ (translate list) ++ ")"

      MakeCountList min_ max_ ->
        "countlist(" ++ (translate min_) ++ ", " ++ (translate max_) ++ ")"

      MakeSimpleList content ->
        "{" ++ (intercalate "," $ map translate content) ++ "}"

      Block i ->
        "(" ++ (translate i) ++ ")"

      --"if(" ++ (translate statif) ++ "){" ++ (translate statthen) ++ ";}else{" ++ (translate statelse) ++ ";};"
      MakeCondition statif statthen statelse ->
        "condition(" ++ (translate statif) ++ "," ++ (translate statthen) ++ "," ++ (translate statelse) ++ ")"

      Function name args body ->
        let
          strname = translate name
        in
          if strname /= "main"
            then
              "auto " ++ (translate name) ++ " = [](" ++ (intercalate "," $ map (\x -> "auto "++(translate x)) args) ++ "){ return " ++ (translate body) ++ "; }"

            else
              "int main( int argc, char** argv){ return " ++ (translate body) ++ "; }"

      Lambda args body ->
        "[](" ++ (intercalate "," $ map (\x -> "auto "++(translate x)) args) ++ "){ return " ++ (translate body) ++ "; }"

      CallFunction name args ->
        (translate name) ++ "(" ++ (intercalate "," $ map translate args) ++ ")"

      Error msg ->
        error msg

