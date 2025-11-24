module Semantico where

import AST

-- ==========================================================================
-- PARTE 1: Configuração da Mônada
-- ==========================================================================

-- Definimos 'Analise' como um sinônimo de IO. 
-- Isso permite "imprimir e continuar".
type Analise a = IO a

aviso :: String -> Analise ()
aviso msg = putStrLn ("ADVERTENCIA: " ++ msg)

erroFatal :: String -> Analise a
erroFatal msg = error ("ERRO: " ++ msg)

-- ==========================================================================
-- PARTE 2: Tabelas de Símbolos
-- ==========================================================================

-- TFun = [(nome, ([lista de tipos dos argumentos], tipo do retorno))]
type TFun = [(String, ([Tipo], Tipo))]
-- TLoc = [(nome, tipo da variavel)]
type TLoc = [(String, Tipo)]

-- ==========================================================================
-- PARTE 3: Funções
-- ==========================================================================

-- funcao pra ver se a variavel existe, retorna o tipo se existir 
lookupVar :: TLoc -> String -> Analise Tipo

lookupVar [] nome = do  
    erroFatal ("Variavel" ++ nome ++ "nao encontrada")

lookupVar ((n, t):xs) nome
    | n == nome = return t
    | otherwise = lookupVar xs nome

-- funcao de verificacao de expressao
verExpr :: TFun -> TLoc -> Expr -> Analise (Tipo, Expr)
-- so retorna o tipo das constantes
verExpr _ _ (Const (CInt n)) = return (TInt, Const(CInt n))
verExpr _ _ (Const (CDouble n)) = return (TDouble, Const(CDouble n))
verExpr _ _ (Const (CString n)) = return (TString, Const(CString n))


verExpr _ tloc (IdVar nome) = do
    t <- lookupVar tloc nome
    return (t, IdVar nome)

-- ==========================================================================
-- OPERACOES ARITMETICAS
-- ==========================================================================

-- soma
verExpr tfun tloc (Add e1 e2) = do
    (t1, e1Corrigida) <- verExpr tfun tloc e1
    (t2, e2Corrigida) <- verExpr tfun tloc e2

    -- soma com um int e um double
    if t1 == TInt && t2 == TDouble then 
        return (TDouble, (Add (IntDouble e1Corrigida) e2Corrigida))
    -- soma com um double e um int
    else if t1 == TDouble && t2 == TInt then
        return (TDouble, (Add e1Corrigida (IntDouble e2Corrigida)))
    -- tentativa de operacao de soma com strings
    else if t1 == TString || t2 == TString then do
        erroFatal "Strings so podem ser usadas em expressoes relacionais!"
    -- caso onde ambos tenham o mesmo tipo
    else if t1 == t2 then
        return (t1, Add e1Corrigida e2Corrigida)
    else do
        erroFatal ("Tipos incompativeis.")

-- subtracao
verExpr tfun tloc (Sub e1 e2) = do
    (t1, e1Corrigida) <- verExpr tfun tloc e1
    (t2, e2Corrigida) <- verExpr tfun tloc e2

    -- subtracao com um int e um double
    if t1 == TInt && t2 == TDouble then 
        return (TDouble, (Sub (IntDouble e1Corrigida) e2Corrigida))
    -- subtracao com um double e um int
    else if t1 == TDouble && t2 == TInt then
        return (TDouble, (Sub e1Corrigida (IntDouble e2Corrigida)))
    -- caso envolva string
    else if t1 == TString || t2 == TString then do
        erroFatal "Strings so podem ser usadas em expressoes relacionais!"
    -- caso onde ambos tenham o mesmo tipo
    else if t1 == t2 then
        return (t1, Sub e1Corrigida e2Corrigida)
    else do
        erroFatal "Tipos incompativeis"

-- multiplicacao
verExpr tfun tloc (Mul e1 e2) = do
    
    (t1, e1Corrigida) <- verExpr tfun tloc e1
    (t2, e2Corrigida) <- verExpr tfun tloc e2

    if t1 == TInt && t2 == TDouble then
        return (TDouble, Mul (IntDouble e1Corrigida) e2Corrigida)
    else if t1 == TDouble && t2 == TInt then
        return (TDouble, Mul e1Corrigida (IntDouble e2Corrigida))
    else if t1 == TString || t2 == TString then do
        erroFatal "Strings so podem ser usadas em expressoes relacionais!"
    else if t1 == t2 then
        return (t1, Mul e1Corrigida e2Corrigida)
    else do
        erroFatal "Tipos incompativeis"

-- divisao
verExpr tfun tloc (Div e1 e2) = do

    (t1, e1Corrigida) <- verExpr tfun tloc e1
    (t2, e2Corrigida) <- verExpr tfun tloc e2

    if t1 == TInt && t2 == TDouble then
        return (TDouble, Div (IntDouble e1Corrigida) e2Corrigida)
    else if t1 == TDouble && t2 == TInt then
        return (TDouble, Div e1Corrigida (IntDouble e2Corrigida))
    else if t1 == TString || t2 == TString then do
        erroFatal "Strings so podem ser usadas em expressoes relacionais!"
    else if t1 == t2 then
        return (t1, Div e1Corrigida e2Corrigida)
    else do
        erroFatal "Tipos incompativeis"

-- ==========================================================================
-- EXPRESSOES RELACIONAIS
-- ==========================================================================

verExprR :: TFun -> TLoc -> ExprR -> Analise (Tipo, ExprR)

--igual
verExprR tfun tloc (Req e1 e2) = do
    
    (t1, e1') <- verExpr tfun tloc e1
    (t2, e2') <- verExpr tfun tloc e2

    if t1 == TInt && t2 == TDouble then
        return (TInt, Req (IntDouble e1') e2')
    else if t1 == TDouble && t2 == TInt then
        return (TInt, Req e1' (IntDouble e2'))
    else if t1 == t2 then
        return (TInt, Req e1' e2')
    else do
        erroFatal ("Os tipos das expressoes utilizadas sao incompativeis: " ++ show t1 ++ " " ++ show t2)

-- diferente
verExprR tfun tloc (Rdif e1 e2) = do

    (t1, e1') <- verExpr tfun tloc e1
    (t2, e2') <- verExpr tfun tloc e2

    if t1 == TInt && t2 == TDouble then
        return (TInt, Rdif (IntDouble e1') e2')
    else if t1 == TDouble && t2 == TInt then
        return (TInt, Rdif e1' (IntDouble e2'))
    else if t1 == t2 then
        return (TInt, Rdif e1' e2')
    else do
        erroFatal ("Os tipos das expressoes utilizadas sao incompativeis: " ++ show t1 ++ " " ++ show t2)

-- menor que -
verExprR tfun tloc (Rlt e1 e2) = do

    (t1, e1') <- verExpr tfun tloc e1
    (t2, e2') <- verExpr tfun tloc e2

    if t1 == TString || t2 == TString then
        erroFatal "Nao e possivel fazer comparacao de >, <, >=, <= com Strings."
    else if t1 == TDouble && t2 == TInt then
        return (TInt, Rlt e1' (IntDouble e2'))
    else if t1 == t2 then
        return (TInt, Rlt e1' e2')
    else if t1 == TInt && t2 == TDouble then
        return (TInt, Rlt (IntDouble e1') e2')
    else do
        erroFatal "nao e possivel relacionar expressoes de tipos diferentes."

-- maior que -
verExprR tfun tloc (Rgt e1 e2) = do

    (t1, e1') <- verExpr tfun tloc e1
    (t2, e2') <- verExpr tfun tloc e2

    if t1 == TString || t2 == TString then
        erroFatal "Nao e possivel fazer comparacao de >, <, >=, <= com Strings."
    else if t1 == TDouble && t2 == TInt then
        return (TInt, Rgt e1' (IntDouble e2'))
    else if t1 == t2 then
        return (TInt, Rgt e1' e2')
    else if t1 == TInt && t2 == TDouble then
        return (TInt, Rgt (IntDouble e1') e2')
    else do
        erroFatal ("Os tipos das expressoes utilizadas sao incompativeis: " ++ show t1 ++ " " ++ show t2)

-- menor igual -
verExprR tfun tloc (Rle e1 e2) = do

    (t1, e1') <- verExpr tfun tloc e1
    (t2, e2') <- verExpr tfun tloc e2

    if t1 == TString || t2 == TString then
        erroFatal "Nao e possivel fazer comparacao de >, <, >=, <= com Strings."
    else if t1 == TDouble && t2 == TInt then
        return (TInt, Rle e1' (IntDouble e2'))
    else if t1 == t2 then
        return (TInt, Rle e1' e2')
    else if t1 == TInt && t2 == TDouble then
        return (TInt, Rle (IntDouble e1') e2')
    else do
        erroFatal ("Os tipos das expressoes utilizadas sao incompativeis: " ++ show t1 ++ " " ++ show t2)

-- maior igual -
verExprR tfun tloc (Rge e1 e2) = do

    (t1, e1') <- verExpr tfun tloc e1
    (t2, e2') <- verExpr tfun tloc e2

    if t1 == TString || t2 == TString then
        erroFatal "Nao e possivel fazer comparacao de >, <, >=, <= com Strings."
    else if t1 == TDouble && t2 == TInt then
        return (TInt, Rge e1' (IntDouble e2'))
    else if t1 == t2 then
        return (TInt, Rge e1' e2')
    else if t1 == TInt && t2 == TDouble then
        return (TInt, Rge (IntDouble e1') e2')
    else do
        erroFatal ("Os tipos das expressoes utilizadas sao incompativeis: " ++ show t1 ++ " " ++ show t2) 

-- ==========================================================================
-- EXPRESSOES LÓGICAS
-- ==========================================================================

verExprL :: TFun -> TLoc -> ExprL -> Analise (Tipo, ExprL)

-- isso aq ta basicamente delegando a funcao pro verificador de expressao relacional ja que numa expressao logica podemos ter uma expressao relacional, ex: if((x > 2) && (y < 3) then...) só o verExprL n consegue tankar o x > 2 nem o y < 3
verExprL tfun tloc (Rel r) = do
    
    (t, r') <- verExprR tfun tloc r

    return (t, Rel r')

-- and
verExprL tfun tloc (And e1 e2) = do

    (t1, e1') <- verExprL tfun tloc e1
    (t2, e2') <- verExprL tfun tloc e2

    if t1 == TInt && t2 == TInt then
        return (TInt, And e1' e2')
    else 
        erroFatal ("Tipos incompativeis para AND (&&). Esperado Int. Recebido: " ++ show t1 ++ " e " ++ show t2)

-- or
verExprL tfun tloc (Or e1 e2) = do

    (t1, e1') <- verExprL tfun tloc e1
    (t2, e2') <- verExprL tfun tloc e2

    if t1 == TInt && t2 == TInt then
        return (TInt, Or e1' e2')
    else 
        erroFatal ("Tipos incompativeis para OR (||). Esperado Int. Recebido: " ++ show t1 ++ " e " ++ show t2)

-- not
verExprL tfun tloc (Not e1) = do

    (t1, e1') <- verExprL tfun tloc e1

    if t1 == TInt then
        return (TInt, Not e1')
    else
        erroFatal ("Tipo incompativel para NOT (1). Esperado Int. Recebido: " ++ show t1)




