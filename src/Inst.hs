{-
    The MIT License (MIT)
    
    Copyright (c) 2015 Mário Feroldi Filho

    Permission is hereby granted, free of charge, to any person obtaining a copy
    of this software and associated documentation files (the "Software"), to deal
    in the Software without restriction, including without limitation the rights
    to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
    copies of the Software, and to permit persons to whom the Software is
    furnished to do so, subject to the following conditions:

    The above copyright notice and this permission notice shall be included in all
    copies or substantial portions of the Software.

    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
    IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
    FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
    AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
    LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
    OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
    SOFTWARE.
-}

module Inst where

type Insts = [Inst]

data Inst
  = PushVar String                --
  | PushConst String              --
  | PushConstf String             --
  | PushChar Char                 --
  | PushString String             --
  | DeclVar String                --
  | AssignTo Inst Inst            --
  | Operation String Inst Inst    --
  | AndInst Inst Inst             --
  | OrInst Inst Inst              --
  | ForList Inst Inst Inst        -- Result; Ranges; Condition.
  | Range Inst Inst               -- x in list
  | MakeCountList Inst Inst       --
  | MakeSimpleList [Inst]         --
  | Block Inst                    --
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
  | Ignore                        -- underline (_)
  | TryInst Inst                  --
  | MatchInst Inst [Inst] Inst    --
  | Cast Inst String              --
  | Error String                  -- For format errors
  deriving (Show, Read, Eq)