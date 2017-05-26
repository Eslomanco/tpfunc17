-- LABORATORIO DE PROGRAMACION FUNCIONAL 2017
-- MODULO DE GENERACION DE CODIGO C

-- Se debe implementar la funcion genProgram que
-- dado un AST que representa un programa valido
-- genera el codigo C correspondiente


module Generator where

import Syntax
-- se pueden agregar mas importaciones 
-- en caso de ser necesario


-- CODE GENERATOR


genProgram :: Program -> String
genProgram _ = "¡Generador en obras!"

--genProgram (Program dfs expr)  = "#include <stdio.h>\n" ++ (genDfs dfs) ++ "\nint main(){" ++ (genExpr expr) ++ "}" -- Implementar

{--
genDfs :: Defs -> String
genDfs _ = ""

genExpr :: Expr -> String
genExpr (Var x) = show x ++ " "
genExpr (IntLit x) =  show x ++ " "
genExpr (BoolLit x) = show x ++ " "
genExpr (Infix op x y) = (genExpr x) ++ " " ++ show op ++ " " ++ (genExpr y)
--}
