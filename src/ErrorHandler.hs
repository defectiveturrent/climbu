module ErrorHandler where

data ExpException
  = IncompleteOperation
  | IncompleteDoBlock
  | IncompleteIfBlock
  | BadList
  | BadMethod
  | BadFunctionDeclaration
  | BadVarDeclaration
  | BadLambdaDeclaration
  | UnknownToken
  | ExcessRubbish
  | SyntaxError
  deriving(Show, Read, Eq)


showEE IncompleteOperation msg = "Incomplete operation: " ++ msg
showEE IncompleteDoBlock msg = "Incomplete do-in: " ++ msg
showEE IncompleteIfBlock msg = "Incomplete if-block: " ++ msg
showEE BadList msg = "Bad list: " ++ msg
showEE BadMethod msg = "Bad method: " ++ msg
showEE BadFunctionDeclaration msg = "Bad function declaration: " ++ msg
showEE BadVarDeclaration msg = "Bad variable declaration: " ++ msg
showEE BadLambdaDeclaration msg = "Bad lambda declaration: " ++ msg
showEE UnknownToken msg = "Unknown token: " ++ msg
showEE ExcessRubbish msg = "Excess rubbish on stack: "
showEE SyntaxError msg = "Unknown syntax: " ++ msg

report :: ExpException -> String -> String -> t
report e al wh = error $ mkmsg e al wh

mkmsg e al wh = showEE e (al ++ "\n\tat: " ++ wh ++ "\n")