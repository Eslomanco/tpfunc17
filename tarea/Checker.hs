-- LABORATORIO DE PROGRAMACION FUNCIONAL 2017
-- MODULO DE CHEQUEO

-- Se debe implementar la funcion checkProgram que
-- dado un AST que representa un programa
-- retorna una lista de mensajes de error en caso
-- de que el programa no sea correcto u Ok en otro caso.  


module Checker where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario


-- CHECKER

data Checked = Ok | Wrong [Error]

data Error = Duplicated      Name
           | Undefined       Name
           | ArgNumDef       Name Int Int
           | ArgNumApp       Name Int Int
           | Expected        Type Type
           
            
instance Show Error where
 show (Duplicated      n)  = "Duplicated declaration: " ++ n
 show (Undefined       n)  = "Undefined: " ++ n
 show (ArgNumDef   n s d)  = "The number of parameters in the definition of " ++ n ++
                             " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (ArgNumApp   n s d)  = "The number of arguments in the application of: " ++ n ++
                             " doesn't match the signature ("++ show d ++ " vs " ++ show s ++ ")"
 show (Expected    ty ty') = "Expected: " ++ show ty ++ " Actual: " ++ show ty'


checkProgram :: Program -> Checked
checkProgram (Program defs body) = checkParamNums defs -- Impementar

-- Checkeo de Duplicados
pertenece :: Eq a => a -> [a] -> Bool
pertenece x [] = False
pertenece x (y:ys) | x == y = True
                   | otherwise = pertenece x ys

listarDuplicados :: [Name] -> [Error]
listarDuplicados [] = []
listarDuplicados (x:xs) | pertenece x xs = (Duplicated x) : listarDuplicados xs
                        | otherwise = listarDuplicados xs

listFuncName :: Defs -> [Name]
listFuncName x = [y | (FunDef (y,_) _ _) <- x]

listVarsName :: Defs -> [[Name]]
listVarsName x = [y | (FunDef _ y _) <- x]

checkDupFunc :: Defs -> [Error]
checkDupFunc x = listarDuplicados $ listFuncName x

checkDupVars :: Defs -> [Error]
checkDupVars x = concat $ map (listarDuplicados) (listVarsName x)

checkDupDecl x | not $ null y = Wrong (y)
               | otherwise = Ok
                where 
                    y = (checkDupFunc x) ++ (checkDupVars x)

-- Número de parámetros

contarParamSig :: Sig -> Int
contarParamSig (Sig x _) = length x

contarParamEq ::  [Name] -> Int
contarParamEq y = length y

cmpParamNum :: FunDef -> [Error]
cmpParamNum (FunDef (name,sig) eqVars _)   | numSig == numEqVars = []
                                           | otherwise = [ArgNumDef name numSig numEqVars]
                                                where
                                                    numSig = contarParamSig sig
                                                    numEqVars = contarParamEq eqVars
                                                
checkParamNums :: Defs -> Checked
checkParamNums x | not $ null y = Wrong y
                 | otherwise = Ok
                 where 
                    y = concat $ map (cmpParamNum) x 

