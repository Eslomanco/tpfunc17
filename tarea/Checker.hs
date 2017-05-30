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
checkProgram (Program defs body) = checkProgramUndefined (Program defs body) -- Impementar

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

-- Nombres no declarados

checkUndefinedVarExpr :: [Name] -> Expr -> [Error]
checkUndefinedVarExpr _ (IntLit _) = []
checkUndefinedVarExpr _ (BoolLit _) = []
checkUndefinedVarExpr xs (Var x) | pertenece x xs = []
                                 | otherwise = [Undefined x]
checkUndefinedVarExpr xs (Infix _ der izq) = (checkUndefinedVarExpr xs der) ++ (checkUndefinedVarExpr xs izq)
checkUndefinedVarExpr xs (If x y z) = (checkUndefinedVarExpr xs x) ++ (checkUndefinedVarExpr xs y) ++ (checkUndefinedVarExpr xs z)
checkUndefinedVarExpr xs (Let (name, _) x1 x2) = (checkUndefinedVarExpr xs x1) ++ (checkUndefinedVarExpr (name:xs) x2)
checkUndefinedVarExpr xs (App name ys) = (if (pertenece name xs) then [] else [Undefined name]) ++ (concat $ map (checkUndefinedVarExpr xs) ys)

checkDefUndefined :: [Name] -> FunDef -> [Error]
checkDefUndefined xs (FunDef _ ys expr) = checkUndefinedVarExpr (xs ++ ys) expr

checkAllDefsUndefined :: [Name] -> Defs -> [Error]
checkAllDefsUndefined ns ds = concat $ map (checkDefUndefined ns) ds

checkProgramUndefined :: Program -> Checked
checkProgramUndefined (Program defs expr)   | null $ auxLista = Ok
                                            | otherwise = Wrong auxLista
                                            where
                                                auxLista = (checkAllDefsUndefined y defs) ++ (checkUndefinedVarExpr y expr)
                                                y = listFuncName defs
                                            
-- Checkeo de tipos

getFuncTypedFun :: FunDef -> TypedFun
getFuncTypedFun (FunDef x _ _) = x

getFuncType :: TypedFun -> Type
getFuncType (_, (Sig _ x)) = x

getFuncTypeList :: TypedFun -> [Type]
getFuncTypeList (_,(Sig xs _)) = xs

getFuncTypeName :: TypedFun -> Name
getFuncTypeName (x, _) = x

getVarName :: TypedVar -> Name
getVarName (x, _) = x

getVarType :: TypedVar -> Type
getVarType (_, x) = x

getFuncTypeFromList :: [TypedFun] -> Name -> Type
getFuncTypeFromList (x:xs) y | getFuncTypeName $ x == y = getFuncType x
                             | otherwise = getFuncTypeFromList xs y

getVarTypeFromList :: [TypedVar] -> Name -> Type
getVarTypeFromList (x:xs) y | getVarName $ x == y = getVarType x
                            | otherwise = getVarTypeFromList xs y

                             
updateVarType :: [TypedVar] -> TypedVar -> [TypedVar]
updateVarType [] x = [x]
updateVarType (y:ys) x | getVarName $ x == getVarName $ y = (x:ys)
                       | otherwise = (y:(updateVarType ys x))

getExprType :: [TypedFun] -> [TypedVar] -> Expr -> Type
getExprType _ _ (IntLit _) = TyInt
getExprType _ _ (BoolLit _) = TyBool
getExprType _ _ (Infix x _ _) = case x of
                                    Add, Sub, Mult, Div -> TyInt
                                    Eq, NEq, GTh, LTh, GEq, LEq -> TyBool
getExprType xs _ (App x _) = getFuncTypeFromList xs x
getExprType _ xs (Var x) = getVarTypeFromList xs x
getExprType fs vs (If _ x _) = getExprType fs vs x
getExprType fs vs (Let ux _ expr) = getExprType fs (updateVarType vs ux) expr










