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
  = Var String
  | Num String
  | AssignTo Inst Inst
  | AddTo Inst Inst
  | SubTo Inst Inst
  | MulTo Inst Inst
  | DivTo Inst Inst
  | ModTo Inst Inst
  | GreaterThan Inst Inst
  | LessThan Inst Inst
  | EqualThan Inst Inst
  | EachTo Inst Inst
  | MakeCountList Inst Inst
  | MakeSimpleList [Inst]
  | Block Inst
  | MakeCondition Inst Inst Inst
  | Function Inst [Inst] Inst     -- auto foo = [](auto a, auto b){ return a + b; }
  | Lambda  [Inst] Inst           -- [](auto a, auto b){ }
