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
  | PushString String
  | DeclVar String
  | AssignTo Inst Inst
  | Operation String Inst Inst
  | ForList Inst Inst Inst Inst -- Var; Result; Ranges; Condition.
  | Range Inst Inst               -- x in list
  | MakeCountList Inst Inst
  | MakeSimpleList [Inst]
  | Block Inst
  | MakeCondition Inst Inst Inst
  | Function Inst [Inst] Inst     -- auto foo = [](auto a, auto b){ return a + b; }
  | Lambda [Inst] Inst            -- [](auto a, auto b){ }
  | CallFunction Inst [Inst]      -- foo (1, x, "Hey")
  | DoTake Inst Inst              -- list take n
  | ConcatList Inst Inst          -- [1, 2] ++ [3, 4]
  | DoStack [Inst]               --
  | TupleInst [Inst]              --
  | ListPMInst [Inst] Inst        --
  | ImportInst String             --
  | NegateInst Inst               --
  | TNothing                      -- For empty places
  | Ignore                        -- _
  | Error String                  -- For format errors
  deriving (Show, Read, Eq)

replace wt ch list
  = map (\x -> if x == wt then ch else x) list

parseAst (Void) = TNothing
parseAst (Decl x) = DeclVar x
parseAst (Call (Num a) [(Num b)]) = PushConst $ (show a) ++ "." ++ (show b) ++ "f"
parseAst (Ident "_") = Ignore
parseAst (Ident x) = PushVar x
parseAst (CharString x) = CallFunction (PushVar "mkstr") [PushString x]
parseAst (CharByte x) = PushChar x
parseAst (Num x) = PushConst (show x)
parseAst (Assign e1 e2) = AssignTo (parseAst e1) (parseAst e2)
parseAst (Take e1 e2) = DoTake (parseAst e1) (parseAst e2)
parseAst (Expo e1 e2) = CallFunction (PushVar "pow") [parseAst e1, parseAst e2]
parseAst (Concat e1 e2) = ConcatList (parseAst e1) (parseAst e2)
parseAst (Import e1) = ImportInst $ (replace '.' '/' e1) ++ ".hpp"
parseAst (Negate e1) = NegateInst $ parseAst e1

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

parseAst (In e1 e2) = Range (parseAst e1) (parseAst e2)

parseAst (For e1 e2 e3)
  = let
      (When condition) = e3

      ranges = parseAst e2
      range = (\(Range _ l) -> l) ranges
      var' = (\(Range x _) -> x) ranges
      result' = parseAst e1
      condition' = case condition of
                    Void ->
                      PushConst "true"

                    other ->
                      parseAst condition

    in
      ForList var' (Lambda [var'] result') range (Lambda [var'] condition')

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
parseAst (DoIn e1 e2) = DoStack $ map parseAst (e1 ++ [e2])
parseAst (Tuple e) = TupleInst $ map parseAst e
parseAst (ListPM e1 e2) = ListPMInst (map parseAst e1) (parseAst e2)
parseAst (Special x) = PushVar $ show x
parseAst _ = TNothing


execute :: [Token] -> [String]
execute stack
  = let
      paragraphs :: [Ast]
      paragraphs = parseEofs stack

      declarations :: [(String, Label)]
      declarations
        = defnConsts ++ let
            search [] = []
            search ((Def (Ident name) args body) : rest)
              = (name, searchLabel declarations $ parseAst body) : search rest

            search ((Assign (Decl var) value) : rest)
              = (var, searchLabel declarations $ parseAst value) : search rest

            search (_:rest)
              = search rest
          in
            search paragraphs

      outcode = map parseAst paragraphs

    in
      map (\x ->
            let
              string = (translate declarations x)
            in
              if isPrefixOf "#include" string
                then
                  string
                else
                  string ++ ";") outcode

genCode :: String -> String
genCode stack
  = "#include \"include/prelude.hpp\"\n"
  ++ (intercalate "\n" . execute . tokenRevision . tokenise $ stack)
  ++ "\n\n"
  ++ "int main( int countArgs, char** args )\n"
  ++ "{\n"
  ++ "  return _main(), 0;\n"
  ++ "}"

searchLabel :: [(String, Label)] -> Inst -> Label
searchLabel stack (CallFunction (PushVar name) args)
  = let
      l = getdefn name stack
    in
      case l of
        UnknownLabel -> getLabel (PushVar name)
        _ -> l

searchLabel stack (PushVar var)
  = let
      l = getdefn var stack
    in
      case l of
        UnknownLabel -> getLabel (PushVar var)
        _ -> l

searchLabel stack (MakeSimpleList list)
  = let
      l = foldl
          (\acc inst
             -> case acc of
                  UnknownLabel -> searchLabel stack inst
                  _ -> acc)
          UnknownLabel
          list
    in
      case l of
        UnknownLabel -> SpecialLabel
        _ -> List l

searchLabel _ inst
  = getLabel inst

translate :: [(String, Label)] -> Inst -> String
translate stack inst
  = let
      trans = translate stack
      getLabelString = show . searchLabel stack

    in
    case inst of
      DeclVar x ->
        x

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

      PushString x ->
        "\"" ++ x ++ "\""

      ImportInst path ->
        "#include \"include/" ++ path ++ "\""

      NegateInst x ->
        "(-" ++ trans x ++ ")"

      AssignTo i1 i2 ->
        case i1 of
          TupleInst list ->
            let
              parseTuple _ Ignore = []
              parseTuple n var
                = "auto " ++ (trans var) ++ " = get<" ++ show n ++ ">(" ++ (trans i2) ++ ")"

              vars = [parseTuple n var | n   <- [0..] 
                                       | var <- list ]

            in
              intercalate ";\n" $ filter (not . null) vars

          MakeSimpleList list ->
            let
              parseList _ Ignore = []
              parseList n (PushVar var)
                = trans . AssignTo (DeclVar var) . DoTake i2 . PushConst . show $ n

              vars = [parseList n var | n   <- [0..]
                                      | var <- list ]
            in
              intercalate ";\n" $ filter (not . null) vars

          ListPMInst heads rtail ->
            let
              hInst = trans $ AssignTo (MakeSimpleList heads) i2
              tInst = trans $ AssignTo rtail ( CallFunction (PushVar "takeSince") [i2, PushConst . show . length $ heads] )

            in
              hInst ++ ";\n" ++ tInst

          DeclVar var ->
            (getLabelString i2) ++ " " ++ var ++ " = " ++ (trans i2)

          otherinst ->
            trans i1 ++ " = " ++ trans i2

      Operation op i1 i2 ->
        (trans i1) ++ op ++ (if op == "/" then "(float)" else []) ++ (trans i2)

      -- TOFIX
      ForList var fresult ranges fcondition ->
        trans $ CallFunction (PushVar "eachlist") [fresult, ranges, fcondition]

      MakeCountList min_ max_ ->
        trans $ CallFunction (PushVar "countlist") [min_, max_]

      msl @ (MakeSimpleList content) ->
        getLabelString msl ++ "({" ++ (intercalate "," $ map trans content) ++ "})"

      Block i ->
        "(" ++ (trans i) ++ ")"

      --"if(" ++ (trans statif) ++ "){" ++ (trans statthen) ++ ";}else{" ++ (trans statelse) ++ ";};"
      MakeCondition statif statthen statelse ->
        "((" ++ (trans statif) ++ ")?(" ++ (trans statthen) ++ "):(" ++ (trans statelse) ++ "))"

      Function name args body ->
        let
          strname = trans name
          checkName = if strname == "main" then "_main" else strname

          --line = trans $ AssignTo (PushVar checkName) (Lambda args body)
          line
            =  genGenericPrefix (if args /= [TNothing] then length args else 0)
            ++ getLabelString body
            ++ " "
            ++ checkName
            ++ genGenericArguments stack args
            ++ "\n{ return "
            ++ (trans body)
            ++ "; }"
        in
          line

      Lambda args body ->
        "[&](" ++ (intercalate "," $ map (\x -> "auto "++(trans x)) args) ++ "){ return " ++ (trans body) ++ "; }"

      CallFunction name args ->
        (trans name) ++ "(" ++ (intercalate "," $ map trans args) ++ ")"

      DoTake i1 i2 ->
        (trans i1) ++ "[" ++ (trans i2) ++ "]"

      ConcatList i1 i2 ->
        trans $ CallFunction (PushVar "conc") [i1, i2]

      DoStack expressions ->
        "[&](){ " ++ (intercalate ";" . map trans . init $ expressions) ++ ";\nreturn " ++ (trans $ last expressions) ++ ";}()"

      TupleInst i ->
        trans $ CallFunction (PushVar "make_tuple") i

      Error msg ->
        error msg


genGenericPrefix n
  = let
      prefix = "template<"
      suffix = "> "
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
          " "

genGenericArguments stack args
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
        ++ intercalate ", " (map (\(a, b) -> unwords [a, b]) $ zip classes (map (translate stack) args))
        ++ suffix

    in
      completeArguments



defnConsts
  = [ ("true", BoolLabel)
    , ("false", BoolLabel)
    , ("Undefined", SpecialLabel)
    , ("NaN", SpecialLabel)
    , ("Infinite", SpecialLabel)
    , ("NuL", SpecialLabel)
    , ("NuT", SpecialLabel)
    , ("NuS", SpecialLabel)
    , ("Null", SpecialLabel)
    , ("Void", SpecialLabel)
    ]

defnCallFun
  = [ ("print", SpecialLabel)
    , ("println", SpecialLabel)
    , ("puts", SpecialLabel)
    , ("putc", SpecialLabel)
    , ("mkstr", List CharLabel)
    , ("sum", IntLabel)
    , ("product", IntLabel)
    , ("elem", BoolLabel)
    , ("getLine", List CharLabel)
    ]

getdefn var db
  = let
      gdc (Just c) = c
      gdc Nothing = UnknownLabel

    in
      gdc $ lookup var db

data Label
  = IntLabel
  | FloatLabel
  | CharLabel
  | BoolLabel
  | List Label
  | SpecialLabel
  | UnknownLabel
  deriving(Eq, Read)

instance Show Label
  where
    show IntLabel = "int"
    show FloatLabel = "float"
    show CharLabel = "char"
    show BoolLabel = "bool"
    show (List CharLabel) = "String"
    show (List UnknownLabel) = "auto"
    show (List label) = "List<" ++ show label ++ ">"
    show SpecialLabel = "SpecialDate_t"
    show UnknownLabel = "auto"

getLabel :: Inst -> Label
getLabel expr
  = case expr of
      PushConst _ ->
        IntLabel

      PushChar _ ->
        CharLabel

      MakeSimpleList x ->
        List $ foldl
               (\acc inst
                  -> case acc of
                      UnknownLabel -> getLabel inst
                      _ -> acc)
               UnknownLabel
               x

      MakeCountList _ _ ->
        List IntLabel

      MakeCondition _ x y ->
        if getLabel x == UnknownLabel
          then
            getLabel y
          else
            getLabel x

      ForList _ x _ _ ->
        getLabel x

      ConcatList x _ ->
        getLabel x

      PushVar x ->
        getdefn x defnConsts

      CallFunction (PushVar x) _ ->
        getdefn x defnCallFun

      Lambda _ body ->
        getLabel body

      DoStack x ->
        getLabel $ last x

      Operation operator _ _ ->
        case operator of
          ">" ->
            BoolLabel
          "<" ->
            BoolLabel
          ">=" ->
            BoolLabel
          "<=" ->
            BoolLabel
          "==" ->
            BoolLabel
          "!=" ->
            BoolLabel
          "/" ->
            FloatLabel
          _ ->
            IntLabel

      Block x ->
        getLabel x

      _ ->
        UnknownLabel

typeChecker :: Inst -> String
typeChecker expression
  = show $ getLabel expression