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
  | AssignTo Inst Inst
  | Operation String Inst Inst
  | EachTo Inst Inst
  | MakeCountList Inst Inst
  | MakeSimpleList [Inst]
  | Block Inst
  | MakeCondition Inst Inst Inst
  | Function Inst [Inst] Inst     -- auto foo = [](auto a, auto b){ return a + b; }
  | Lambda  [Inst] Inst           -- [](auto a, auto b){ }
  | CallFunction Inst [Inst]      -- foo (1, x, "Hey")
  deriving (Show, Read, Eq)


parseAst (Ident x) = PushVar x
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
parseAst (ComprehensionList xs) = MakeSimpleList (map parseAst xs)
parseAst (ParenthesesBlock e1) = Block (parseAst e1)
parseAst (Def name args body) = Function (parseAst name) (map parseAst args) (parseAst body)
parseAst (LambdaDef args body) = Lambda (map parseAst args) (parseAst body)
parseAst (Call name args) = CallFunction (parseAst name) (map parseAst args)
parseAst (Condition stat thenStat elseStat) = MakeCondition (parseAst stat) (parseAst thenStat) (parseAst elseStat)
