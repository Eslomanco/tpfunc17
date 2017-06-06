-- LABORATORIO DE PROGRAMACION FUNCIONAL 2017
-- MODULO DE GENERACION DE CODIGO C

-- Se debe implementar la funcion genProgram que
-- dado un AST que representa un programa valido
-- genera el codigo C correspondiente


module Generator where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario

instance Show Op where
    show Add = "+"
    show Sub = "-"
    show Mult = "*"
    show Div = "div"
    show Eq = "="
    show NEq = "/="
    show GTh = ">"
    show LTh = "<"
    show GEq = ">="
    show LEq = "<="

-- CODE GENERATOR


genProgram :: Program -> String


genProgram (Program dfs expr)  = "#include <stdio.h>\n" ++ (generateDefs dfs) ++ "int main() {\nprintf(\"%d\\n\"," ++ (genExpr expr) ++ "); }" -- Implementar


generateDefs :: Defs -> String
generateDefs [] = []
generateDefs (x:[]) = (generateDefinition x) ++ "\n"
generateDefs (x:xs) = (generateDefinition x) ++ (generateDefs xs)


generateDefinition :: FunDef -> String
generateDefinition (FunDef (n,s) nx expr) = "int _" ++ show n ++ "(" ++ name2param nx ++ "){\nreturn (" ++ genExpr expr ++ "); };"

name2param :: [Name] -> String
name2param [] = []
name2param (x:[]) = "int _" ++ (show x)
name2param (x:xs) = "int _" ++ (show x) ++ "," ++ name2param xs

expr2param :: [Expr] -> String
expr2param [] = []
expr2param (x:[]) = (genExpr x)
expr2param (x:xs) = (genExpr x) ++ "," ++ (expr2param xs)

genExpr :: Expr -> String
genExpr (Var x) = "_" ++ show x
genExpr (IntLit x) =  show x 
genExpr (BoolLit x) = if x then "1" else "0" 
genExpr (Infix op x y) = "(" ++ (genExpr x) ++ " " ++ show op ++ " " ++ (genExpr y) ++ ")"
genExpr (If e1 e2 e3) = (genExpr e1) ++ "?" ++ (genExpr e2) ++ ":" ++ (genExpr e3)
genExpr (App n ns) = "_" ++ show n ++ "(" ++ (expr2param ns) ++ ")"
genExpr (Let _ _ _) = "Acá va un let!"
