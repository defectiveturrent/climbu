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
import Token
import Ast
import Inst
import Parser


{-----------------------------
        Code Generator
-----------------------------}

execute :: [Token] -> Either String [String]
execute stack
  = let
      paragraphs :: Either String Asts
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
            case paragraphs of
              Right p
                -> search p
              Left msg
                -> []

      outcode
        = case paragraphs of
            Right p
              -> Right $ map parseAst p
            Left msg
              -> Left msg

    in
      case outcode of
        Right oc
          -> Right $ map (\x ->
                        let
                          string = (translate declarations x)
                        in
                          if isPrefixOf "#include" string
                            then
                              string
                            else
                              string ++ ";") oc

        Left msg
          -> Left msg

genCode :: String -> Either String String
genCode stack
  = case (execute $ parseTokens stack) of
      Right clines
        -> Right $ "#include \"include/prelude.hpp\"\n"
                    ++ intercalate "\n" clines
                    ++ "\n\n"
                    ++ "int main( int countArgs, char** args )\n"
                    ++ "{\n"
                    ++ "  return _main(), 0;\n"
                    ++ "}"

      Left msg
        -> Left msg

fromEither (Right x) = x
fromEither (Left x) = x

searchLabel :: [(String, Label)] -> Inst -> Label
searchLabel stack (CallFunction (PushVar "countlist") [a, b])
  = let
      l = foldl
          (\acc inst
             -> case acc of
                  UnknownLabel -> searchLabel stack inst
                  _ -> acc)
          UnknownLabel
          [a, b]
    in
      case l of
        UnknownLabel -> SpecialLabel
        _ -> List l

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

      PushConstf x ->
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
              parseTuple (_, Ignore) = []
              parseTuple (n, var) = "auto " ++ (trans var) ++ " = get<" ++ show n ++ ">(" ++ (trans i2) ++ ")"

            in
              intercalate ";\n" . filter (not . null) . map parseTuple . zip [0..] $ list

          MakeSimpleList list ->
            let
              parseList (_, Ignore) = []
              parseList (n, PushVar var) = trans . AssignTo (DeclVar var) . DoTake i2 . PushConst . show $ n

            in
              intercalate ";\n" . filter (not . null) . map parseList . zip [0..] $ list

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
      ForList fresult range fcondition ->
        trans $ CallFunction (PushVar "eachlist") [fresult, range, fcondition]

      MakeCountList a b ->
        trans $ CallFunction (PushVar "countlist") [a, b]

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
            ++ "{ return "
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
    , ("Char", CharLabel)
    , ("Int", IntLabel)
    , ("Float", FloatLabel)
    , ("Double", DoubleLabel)
    , ("Bool", BoolLabel)
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
  | DoubleLabel
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
    show DoubleLabel = "double"
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
      Lambda _ _ ->
        UnknownLabel

      PushConst _ ->
        IntLabel

      PushConstf _ ->
        FloatLabel

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

      MakeCountList a b ->
        List $ foldl
               (\acc inst
                  -> case acc of
                      UnknownLabel -> getLabel inst
                      _ -> acc)
               UnknownLabel
               [a, b]

      CallFunction (PushVar "countlist") args ->
        List $ foldl
               (\acc inst
                  -> case acc of
                      UnknownLabel -> getLabel inst
                      _ -> acc)
               UnknownLabel
               args

      MakeCondition _ x y ->
        if getLabel x == UnknownLabel
          then
            getLabel y
          else
            getLabel x

      ForList x _ _ ->
        getLabel x

      ConcatList x _ ->
        getLabel x

      PushVar x ->
        getdefn x defnConsts

      CallFunction (PushVar x) _ ->
        getdefn x defnCallFun

      DoStack x ->
        getLabel $ last x

      Operation operator a b ->
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
            let
              alabel = getLabel a
              blabel = getLabel b
            in
              if alabel == IntLabel
                then
                  if blabel == IntLabel
                    then IntLabel
                    else FloatLabel
                else alabel

      Block x ->
        getLabel x

      _ ->
        UnknownLabel

typeChecker :: Inst -> String
typeChecker expression
  = show $ getLabel expression