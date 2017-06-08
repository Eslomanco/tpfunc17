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


genProgram (Program dfs expr)  = "#include <stdio.h>\n" ++ (generateDefs dfs) ++ "int main() {\n" ++ (fst $ getLetDefinition expr ([],0)) ++ "printf(\"%d\\n\"," ++ (genExpr expr) ++ "); }\n" -- Implementar


generateDefs :: Defs -> String
generateDefs [] = []
generateDefs (x:[]) = (generateDefinition x) ++ "\n"
generateDefs (x:xs) = (generateDefinition x) ++ (generateDefs xs)


generateDefinition :: FunDef -> String
generateDefinition (FunDef (n,s) nx expr) = "int _" ++ n ++ "(" ++ name2param nx ++ "){\n" ++ (fst $ getLetDefinition expr ([],0)) ++ "return (" ++ (genExpr expr) ++ "); };"

name2param :: [Name] -> String
name2param [] = []
name2param (x:[]) = "int _" ++ x
name2param (x:xs) = "int _" ++ x ++ "," ++ name2param xs

expr2param :: [Expr] -> String
expr2param [] = []
expr2param (x:[]) = (genExpr x)
expr2param (x:xs) = (genExpr x) ++ "," ++ (expr2param xs)

genExpr :: Expr -> String
genExpr (Var x) = '_':x
genExpr (IntLit x) =  show x 
genExpr (BoolLit x) = if x then ['1'] else ['0'] 
genExpr (Infix op x y) = "(" ++ (genExpr x) ++ " " ++ show op ++ " " ++ (genExpr y) ++ ")"
genExpr (If e1 e2 e3) = (genExpr e1) ++ "?" ++ (genExpr e2) ++ ":" ++ (genExpr e3)
genExpr (App n ns) = "_" ++ n ++ "(" ++ (expr2param ns) ++ ")"
genExpr (Let _ _ _) = "Let!"


getLetDefinition :: Expr -> (String, Int) -> (String, Int)
getLetDefinition (Var _) x = x 
getLetDefinition (IntLit _) x = x
getLetDefinition (BoolLit _) x = x 
getLetDefinition (Infix op l r) x = getLetDefinition r $ getLetDefinition l x
getLetDefinition (If e1 e2 e3) x = getLetDefinition e3 $ getLetDefinition e2 $ getLetDefinition e1 x 
getLetDefinition (App n (ex:[])) x = getLetDefinition ex x                                       
getLetDefinition (App n (ex:exs)) x = getLetDefinition (App n exs) (getLetDefinition ex x)     

getLetDefinition (Let (n,_) x1 x2) (s,i) = (s ++ strX1 ++ "int _let" ++ (show numX2) ++ "(int _" ++ n ++ "){\n" ++ strX2 ++ "return (" ++ (genExpr x2) ++ "); };\n", numX2 + 1)
                                        where
                                            strX1 = fst $ getLetDefinition x1 ([],i)
                                            numX1 = snd $ getLetDefinition x1 ([],i)
                                            strX2 = fst $ getLetDefinition x2 ([], numX1)
                                            numX2 = snd $ getLetDefinition x2 ([], numX1)
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            