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
    show Add = " + "
    show Sub = " - "
    show Mult = " * "
    show Div = " div "
    show Eq = "=="
    show NEq = "/="
    show GTh = ">"
    show LTh = "<"
    show GEq = ">="
    show LEq = "<="

-- CODE GENERATOR


genProgram :: Program -> String


genProgram (Program dfs expr)  = "#include <stdio.h>\n" ++ (generateDefs dfs) ++ "int main() {\n" ++ (fst $ getLetDefinition expr ([],0)) ++ "printf(\"%d\\n\"," ++ (fst $ genExpr expr 0) ++ "); }\n" -- Implementar


generateDefs :: Defs -> String
generateDefs [] = []
generateDefs (x:[]) = (generateDefinition x)
generateDefs (x:xs) = (generateDefinition x) ++ (generateDefs xs)


generateDefinition :: FunDef -> String
generateDefinition (FunDef (n,s) nx expr) = "int _" ++ n ++ "(" ++ name2param nx ++ "){\n" ++ (fst $ getLetDefinition expr ([],0)) ++ "return (" ++ (fst $ genExpr expr 0) ++ "); };\n"

name2param :: [Name] -> String
name2param [] = []
name2param (x:[]) = "int _" ++ x
name2param (x:xs) = "int _" ++ x ++ "," ++ name2param xs

expr2param :: [Expr] -> Int -> (String, Int)
expr2param [] x = ([],x)
expr2param (x:[]) y = genExpr x y
expr2param (x:xs) y = ((fst $ (genExpr x y)) ++ "," ++ (fst $ (expr2param xs (snd $ genExpr x y))),snd $ expr2param xs (snd $ genExpr x y))

genExpr :: Expr -> Int -> (String, Int)
genExpr (Var x) y = ('_':x, y)
genExpr (IntLit x) y =  (show x, y) 
genExpr (BoolLit x) y = (if x then ['1'] else ['0'], y) 
genExpr (Infix op x y) z = ("(" ++ (strX) ++ show op ++ (strY) ++ ")", numY)
                            where
                                strX = fst $ genExpr x z
                                numX = snd $ genExpr x z
                                strY = fst $ genExpr y numX
                                numY = snd $ genExpr y numX
                                
genExpr (If e1 e2 e3) i = ((strE1) ++ "?" ++ (strE2) ++ ":" ++ (strE3),numE3)
                            where
                                strE1 = fst $ genExpr e1 i
                                numE1 = snd $ genExpr e1 i 
                                strE2 = fst $ genExpr e2 numE1
                                numE2 = snd $ genExpr e2 numE1
                                strE3 = fst $ genExpr e3 numE2
                                numE3 = snd $ genExpr e3 numE2
                            
genExpr (App n ns) y = ("_" ++ n ++ "(" ++ (fst $ (expr2param ns y)) ++ ")", snd $ (expr2param ns y))

genExpr (Let (n,t) x1 x2) y = ("_let" ++ (show numLet) ++ "(" ++ (fst $ genExpr x1 y) ++ ")", numLet + 1)
                                where
                                    numLet = snd $ genExpr x2 (snd $ genExpr x1 y)


getLetDefinition :: Expr -> (String, Int) -> (String, Int)
getLetDefinition (Var _) x = x 
getLetDefinition (IntLit _) x = x
getLetDefinition (BoolLit _) x = x 
getLetDefinition (Infix op l r) x = getLetDefinition r $ getLetDefinition l x
getLetDefinition (If e1 e2 e3) x = getLetDefinition e3 $ getLetDefinition e2 $ getLetDefinition e1 x 
getLetDefinition (App n (ex:[])) x = getLetDefinition ex x                                       
getLetDefinition (App n (ex:exs)) x = getLetDefinition (App n exs) (getLetDefinition ex x)     

getLetDefinition (Let (n,_) x1 x2) (s,i) = (s ++ strX1 ++ "int _let" ++ (show numX2) ++ "(int _" ++ n ++ "){\n" ++ strX2 ++ "return (" ++ (fst $ genExpr x2 numX1) ++ "); };\n", numX2 + 1)
                                        where
                                            strX1 = fst $ getLetDefinition x1 ([],i)
                                            numX1 = snd $ getLetDefinition x1 ([],i)
                                            strX2 = fst $ getLetDefinition x2 ([], numX1)
                                            numX2 = snd $ getLetDefinition x2 ([], numX1)
                                            
                                            
                                            
                                            
                                            
                                            
                                            
                                            
