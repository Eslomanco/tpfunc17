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

-- Número de parámetros en Declaración

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

-- -- Generación de Entornos

updateEnv :: Env -> TypedVar -> Env 
updateEnv xs x = (x:xs)

getDefinedFunctionTypes :: Defs -> [TypedFun]
getDefinedFunctionTypes d = [x | (FunDef x _ _) <- d]

getFunctionEnv :: FunDef -> Env 
getFunctionEnv (FunDef ((_,Sig ds _)) ns _) = zip ns ds

getSignatureTypeList :: Sig -> [Type]
getSignatureTypeList (Sig x _) = x

-- -- Obtención de tipado
getEnvVarType :: Env -> Name -> Type
getEnvVarType ((n,t):xs) y | y == n = t
                           | otherwise = getEnvVarType xs y
                                                  
getFuncType :: [TypedFun] -> Name -> Type
getFuncType ((nf,(Sig _ t)):xs) n   | n == nf = t
                                    | otherwise = getFuncType xs n

getFuncSignature :: [TypedFun] -> Name -> Sig 
getFuncSignature ((ns,x):xs) n  | ns == n = x
                                | otherwise = getFuncSignature xs n


getExprType :: [TypedFun] -> Env -> Expr -> Type
getExprType _ _ (IntLit _) = TyInt
getExprType _ _ (BoolLit _) = TyBool
getExprType fs _ (App fn _) = getFuncType fs fn
getExprType _ vs (Var v) = getEnvVarType vs v
getExprType fs vs (If _ x _) = getExprType fs vs x
getExprType fs vs (Let ux _ expr) = getExprType fs (updateEnv vs ux) expr
getExprType _ _ (Infix x _ _) = case x of
                                    Add -> TyInt
                                    Sub -> TyInt
                                    Mult -> TyInt
                                    Div -> TyInt
                                    Eq -> TyBool 
                                    NEq-> TyBool
                                    GTh-> TyBool
                                    LTh-> TyBool
                                    GEq-> TyBool
                                    LEq -> TyBool


-- -- Control de tipado

checkExprType :: [TypedFun] -> Env -> Expr -> [Error]
checkExprType _ _ (IntLit _) = []
checkExprType _ _ (BoolLit _) = []
checkExprType _ _ (Var _) = []
checkExprType fs vs (Infix op l r) = (checkExprType fs vs l) ++ (checkExprType fs vs r) ++
                                     (case (getExprType fs vs (Infix op l r)) of
                                        TyInt   -> auxCheckTypeOpInt fs vs l r
                                        TyBool  -> auxCheckTypeEquals fs vs l r
                                     )
checkExprType fs vs (If cond thn els) = (if (getExprType fs vs cond) == TyBool then [] else [Expected TyBool TyInt]) ++ (auxCheckTypeEquals fs vs thn els)
checkExprType fs vs (Let (n,t) l r) =   (if t == (getExprType fs vs l) then [] else [Expected t (getExprType fs vs l)]) ++
                                        (checkExprType fs (updateEnv vs (n,t)) r)
checkExprType fs vs (App n exs) = (auxCheckParmNum fs n exs) ++ (auxCheckParmType fs vs n exs) ++ (concat $ map (checkExprType fs vs) exs)

auxCheckTypeOpInt :: [TypedFun] -> Env -> Expr -> Expr -> [Error]
auxCheckTypeOpInt fs vs l r =   (if (getExprType fs vs l) == TyBool then [Expected TyInt TyBool] else []) ++
                                (if (getExprType fs vs r) == TyBool then [Expected TyInt TyBool] else [])

auxCheckTypeEquals :: [TypedFun] -> Env -> Expr -> Expr -> [Error]
auxCheckTypeEquals fs vs l r    | x == y = []
                                | otherwise = [Expected x y]
                                where
                                    x = getExprType fs vs l
                                    y = getExprType fs vs r

auxCheckParmTypeEquals :: [TypedFun] -> Env -> Type -> Expr -> [Error]
auxCheckParmTypeEquals fs vs t expr | t == y = []
                                    | otherwise = [Expected t y]
                                    where 
                                        y = getExprType fs vs expr

auxCheckParmNum fs n exs    | x == y = []
                            | otherwise = [ArgNumApp n y x] 
                            where
                                x = contarParamSig $ getFuncSignature fs n 
                                y = length exs
                                
auxCheckParmType fs vs n exs =  concat $ zipWith (auxCheckParmTypeEquals fs vs) x exs
                                where
                                    x = getSignatureTypeList $ getFuncSignature fs n











