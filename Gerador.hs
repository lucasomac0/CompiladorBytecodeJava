module Gerador where

import AST 
import Control.Monad.State

type Gerador = State Int

-- TFun = [(nome, ([lista de tipos dos argumentos], tipo do retorno))]
type TFun = [(String, ([Tipo], Tipo))]
-- TLoc = [(nome, tipo da variavel)]
type TLoc = [(String, Int)]
-- TabIndices = [(nome, índice)] 
-- necessario pois o gerador vai receber uma tabela de variaveis onde as variaveis estão escritas no formato TLoc descrito acima, sendo que na AST ela está como Id :#: (Tipo, ìndice), mas como no semantico foi facilitado o uso só pra (nome, tipo) aqui preciso de um jeito de salvar os índices de cada variável. Aqui o TabIndices guarda o nome e o índice da variável pois o tipo não vai fazer diferença nessa etapa
type TabIndices = [(String, Int)]

genOp :: Tipo -> String

novoLabel :: Gerador String 
novoLabel = do 
    n <- get
    put (n+1)
    return ("l"++show n)

genCab :: String -> Gerador String
genCab nome = return (".class public " ++ nome ++ 
                    "\n.super java/lang/Object\n\n.method public <init>()V\n\taload_0\n\tinvokenonvirtual java/lang/Object/<init>()V\n\treturn\n.end method\n\n")

genMainCab s l = return (".method public static main([Ljava/lang/String;)V" ++
                        "\n\t.limit stack " ++ show s ++
                        "\n\t.limit locals " ++ show l ++ "\n\n")


genExprL :: String -> TLoc -> TFun -> String -> String -> ExprL -> Gerador String

-- and -- falta testar
genExprL c tab fun v f (And e1 e2) = do 
    l1 <- novoLabel
    e1' <- genExprL c tab fun l1 f e1 -- se e1 for True -> vai p L1 / False -> vai pra falha (f)
    e2' <- genExprL c tab fun v f e2  -- se e2 for True -> vai p sucesso (v) / False -> vai pra falha (f)
    return (e1'++l1++":\n"++e2')

-- or -- falta testar
genExprL c tab fun v f (Or e1 e2) = do
    l1 <- novoLabel
    e1' <- genExprL c tab fun v l1 e1 -- se e1 for true -> vai pra sucesso (v). Se False vai pra L1
    e2' <- genExprL c tab fun v f e2 -- se e2 for true -> vai pra v. Se False vai pra falha total (f)
    return (e1'++l1++":\n"++e2')

-- not -- falta testar
genExprL c tab fun v f (Not e) = do
    e' <- genExprL c tab fun f v e -- se for verdadeiro, pula p falso

-- TODO Rel

genExprR :: String -> TLoc -> TFun -> String -> ExprR -> Gerador String

-- Req -- falta testar
genExprR c tab fun v f (Req e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (e1'++e2'++genRel t1 t2 v "eq"++"\tgoto "++f++"\n")

-- Rdif -- falta testar
genExprR c tab fun v f (Rdif e1 e2) = do
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    

genExpr :: String -> TLoc -> TFun -> Expr -> Gerador String

-- Const 
genExpr c tab fun (Const (CInt i)) = return (TInt, genInt i)
genExpr c tab fun (Add e1 e2) = do 
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e2
    return (t1, e1' ++ e2' ++ genOp t1 "add")

genExpr c tab fun (Sub e1 e2) = do
    (t1, e1') <- genExpr c tab fun e1
    (t2, e2') <- genExpr c tab fun e1
    return (t1, e1' ++ e2' ++ genOp t1 "sub")

genCmd c tab fun (While e b) = do 
    li <- novoLabel
    lv <- novoLabel
    lf <- novoLabel
    e' <- genExprL c tab fun lv lf e
    b' <- genBloco c tab fun b
    return (li++":\n"++e'++lv++":\n"++b'++"\tgoto "++li++"\n"++lf++":\n")

gerar nome p = fst $ runState (genProg nome p) 0