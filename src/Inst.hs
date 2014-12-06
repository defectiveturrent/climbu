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

module Inst where

type Insts = [Inst]

data Inst
  = PushVar String
  | PushConst String
  | PushConstf String
  | PushChar Char
  | PushString String
  | DeclVar String
  | AssignTo Inst Inst
  | Operation String Inst Inst
  | AndInst Inst Inst
  | OrInst Inst Inst
  | ForList Inst Inst Inst        -- Result; Ranges; Condition.
  | Range Inst Inst               -- x in list
  | MakeCountList Inst Inst
  | MakeSimpleList [Inst]
  | Block Inst
  | MakeCondition Inst Inst Inst  -- Cond Then Else
  | Function Inst [Inst] Inst     -- auto foo = [](auto a, auto b){ return a + b; }
  | Lambda [Inst] Inst            -- [](auto a, auto b){ }
  | CallFunction Inst [Inst]      -- foo (1, x, "Hey")
  | DoTake Inst Inst              -- list take n
  | ConcatList Inst Inst          -- [1, 2] ++ [3, 4]
  | DoStack [Inst]                --
  | TupleInst [Inst]              --
  | ListPMInst [Inst] Inst        --
  | ImportInst String             --
  | NegateInst Inst               --
  | TNothing                      -- For empty places
  | Ignore                        -- _
  | Error String                  -- For format errors
  deriving (Show, Read, Eq)