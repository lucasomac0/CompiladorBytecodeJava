module Semantico where

import AST

-- ==========================================================================
-- PARTE 1: Configuração da Mônada
-- ==========================================================================

data Result a = Result (Bool, String, a) deriving Show

instance Functor Result where
    fmap f (Result (b, s, a)) = Result (b, s, f a)

instance Applicative Result where
    pure a = Result (False, "", a)
    Result (b1, s1, f) <*> Result (b2, s2, x) = Result (b1 || b2, s1 <> s2, f x)   

instance Monad Result where 
    Result (b, s, a) >>= f = let Result (b', s', a') = f a in Result (b || b', s++s', a')

errorMsg s = Result (True, "Erro:"++s++"\n", ())
warningMsg s = Result (False, "Advertencia:"++s++"\n", ())

type Analise a = Result a 

aviso :: String -> Analise ()
aviso = warningMsg

erroFatal :: String -> Analise ()
erroFatal = errorMsg

-- ==========================================================================
-- PARTE 2: Tabelas de Símbolos
-- ==========================================================================

-- TFun = [(nome, ([lista de tipos dos argumentos], tipo do retorno))]
type TFun = [(String, ([Tipo], Tipo))]
-- TLoc = [(nome, tipo da variavel)]
type TLoc = [(String, Tipo)]

-- ==========================================================================
-- PARTE 3: Funções de lookup
-- ==========================================================================

-- funcao pra ver se a variavel existe, retorna o tipo se existir 
lookupVar :: TLoc -> String -> Analise Tipo
lookupVar [] nome = do  
    erroFatal ("Variavel " ++ nome ++ " nao encontrada")
    return TVoid

lookupVar ((n, t):xs) nome
    | n == nome = return t
    | otherwise = lookupVar xs nome

lookupFun :: TFun -> String -> Analise ([Tipo], Tipo)
lookupFun [] nome = do
    erroFatal ("Funcao '" ++ nome ++ "' nao declarada.")
    return ([], TVoid) -- retorno dummy

lookupFun ((n, d):xs) nome
    | n == nome = return d
    | otherwise = lookupFun xs nome

-- ==========================================================================
-- VERIFICACAO DE EXPRESSOES
-- ==========================================================================

verExpr :: TFun -> TLoc -> Expr -> Analise (Tipo, Expr)
-- so retorna o tipo das constantes
verExpr _ _ (Const (CInt n)) = return (TInt, Const(CInt n))
verExpr _ _ (Const (CDouble n)) = return (TDouble, Const(CDouble n))
verExpr _ _ (Const (CString n)) = return (TString, Const(CString n))

verExpr _ tloc (IdVar nome) = do
    t <- lookupVar tloc nome
    return (t, IdVar nome)

-- literal string
verExpr _ _ (Lit s) = return (TString, Lit s)

-- negacao unaria neg
verExpr tfun tloc (Neg e) = do
    (t, e') <- verExpr tfun tloc e
    if t == TInt || t == TDouble then
        return (t, Neg e')
    else do
        erroFatal ("O operador unario (-) so pode ser aplicado a numeros. Recebido: " ++ show t)
        return (t, Neg e')

-- chamada de funcao
verExpr tfun tloc (Chamada id args) = do
    -- busca a assinatura da função
    (tiposEsperados, tipoRetorno) <- lookupFun tfun id
    
    -- verifica número de argumentos
    if length args /= length tiposEsperados then do
        erroFatal ("Numero incorreto de argumentos para '" ++ id ++ "'.")
        return (tipoRetorno, Chamada id args)
    else do
        -- verifica os tipos dos argumentos (reusa a funcao verificarArgs)
        argsCorrigidos <- verificarArgs tfun tloc tiposEsperados args
        return (tipoRetorno, Chamada id argsCorrigidos)

-- OPERACOES ARITMETICAS

verExpr tfun tloc (Add e1 e2) = do
    (t1, e1Corrigida) <- verExpr tfun tloc e1
    (t2, e2Corrigida) <- verExpr tfun tloc e2

    if t1 == TInt && t2 == TDouble then 
        return (TDouble, (Add (IntDouble e1Corrigida) e2Corrigida))
    else if t1 == TDouble && t2 == TInt then
        return (TDouble, (Add e1Corrigida (IntDouble e2Corrigida)))
    else if t1 == TString || t2 == TString then do
        erroFatal "Strings so podem ser usadas em expressoes relacionais!"
        return (TVoid, Add e1Corrigida e2Corrigida)
    else if t1 == t2 then
        return (t1, Add e1Corrigida e2Corrigida)
    else do
        erroFatal ("Tipos incompativeis.")
        return (TVoid, Add e1Corrigida e2Corrigida)

verExpr tfun tloc (Sub e1 e2) = do
    (t1, e1Corrigida) <- verExpr tfun tloc e1
    (t2, e2Corrigida) <- verExpr tfun tloc e2

    if t1 == TInt && t2 == TDouble then 
        return (TDouble, (Sub (IntDouble e1Corrigida) e2Corrigida))
    else if t1 == TDouble && t2 == TInt then
        return (TDouble, (Sub e1Corrigida (IntDouble e2Corrigida)))
    else if t1 == TString || t2 == TString then do
        erroFatal "Strings so podem ser usadas em expressoes relacionais!"
        return (TVoid, Sub e1Corrigida e2Corrigida)
    else if t1 == t2 then
        return (t1, Sub e1Corrigida e2Corrigida)
    else do
        erroFatal "Tipos incompativeis"
        return (TVoid, Sub e1Corrigida e2Corrigida)

verExpr tfun tloc (Mul e1 e2) = do
    (t1, e1Corrigida) <- verExpr tfun tloc e1
    (t2, e2Corrigida) <- verExpr tfun tloc e2

    if t1 == TInt && t2 == TDouble then
        return (TDouble, Mul (IntDouble e1Corrigida) e2Corrigida)
    else if t1 == TDouble && t2 == TInt then
        return (TDouble, Mul e1Corrigida (IntDouble e2Corrigida))
    else if t1 == TString || t2 == TString then do
        erroFatal "Strings so podem ser usadas em expressoes relacionais!"
        return (TVoid, Mul e1Corrigida e2Corrigida)
    else if t1 == t2 then
        return (t1, Mul e1Corrigida e2Corrigida)
    else do
        erroFatal "Tipos incompativeis"
        return (TVoid, Mul e1Corrigida e2Corrigida)

verExpr tfun tloc (Div e1 e2) = do
    (t1, e1Corrigida) <- verExpr tfun tloc e1
    (t2, e2Corrigida) <- verExpr tfun tloc e2

    if t1 == TInt && t2 == TDouble then
        return (TDouble, Div (IntDouble e1Corrigida) e2Corrigida)
    else if t1 == TDouble && t2 == TInt then
        return (TDouble, Div e1Corrigida (IntDouble e2Corrigida))
    else if t1 == TString || t2 == TString then do
        erroFatal "Strings so podem ser usadas em expressoes relacionais!"
        return (TVoid, Div e1Corrigida e2Corrigida)
    else if t1 == t2 then
        return (t1, Div e1Corrigida e2Corrigida)
    else do
        erroFatal "Tipos incompativeis"
        return (TVoid, Div e1Corrigida e2Corrigida)

-- ==========================================================================
-- EXPRESSOES RELACIONAIS (já existentes)
-- ==========================================================================

verExprR :: TFun -> TLoc -> ExprR -> Analise (Tipo, ExprR)
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
        return (TVoid, Req e1' e2')

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
        return (TVoid, Rdif e1' e2')

verExprR tfun tloc (Rlt e1 e2) = do
    (t1, e1') <- verExpr tfun tloc e1
    (t2, e2') <- verExpr tfun tloc e2

    if t1 == TString || t2 == TString then do
        erroFatal "Nao e possivel fazer comparacao de >, <, >=, <= com Strings."
        return (TVoid, Rlt e1' e2')
    else if t1 == TDouble && t2 == TInt then
        return (TInt, Rlt e1' (IntDouble e2'))
    else if t1 == t2 then
        return (TInt, Rlt e1' e2')
    else if t1 == TInt && t2 == TDouble then
        return (TInt, Rlt (IntDouble e1') e2')
    else do
        erroFatal "nao e possivel relacionar expressoes de tipos diferentes."
        return (TVoid, Rlt e1' e2')

verExprR tfun tloc (Rgt e1 e2) = do
    (t1, e1') <- verExpr tfun tloc e1
    (t2, e2') <- verExpr tfun tloc e2

    if t1 == TString || t2 == TString then do
        erroFatal "Nao e possivel fazer comparacao de >, <, >=, <= com Strings."
        return (TVoid, Rgt e1' e2')
    else if t1 == TDouble && t2 == TInt then
        return (TInt, Rgt e1' (IntDouble e2'))
    else if t1 == t2 then
        return (TInt, Rgt e1' e2')
    else if t1 == TInt && t2 == TDouble then
        return (TInt, Rgt (IntDouble e1') e2')
    else do
        erroFatal ("Os tipos das expressoes utilizadas sao incompativeis: " ++ show t1 ++ " " ++ show t2)
        return (TVoid, Rgt e1' e2')

verExprR tfun tloc (Rle e1 e2) = do
    (t1, e1') <- verExpr tfun tloc e1
    (t2, e2') <- verExpr tfun tloc e2

    if t1 == TString || t2 == TString then do
        erroFatal "Nao e possivel fazer comparacao de >, <, >=, <= com Strings."
        return (TVoid, Rle e1' e2')
    else if t1 == TDouble && t2 == TInt then
        return (TInt, Rle e1' (IntDouble e2'))
    else if t1 == t2 then
        return (TInt, Rle e1' e2')
    else if t1 == TInt && t2 == TDouble then
        return (TInt, Rle (IntDouble e1') e2')
    else do
        erroFatal ("Os tipos das expressoes utilizadas sao incompativeis: " ++ show t1 ++ " " ++ show t2)
        return (TVoid, Rle e1' e2')

verExprR tfun tloc (Rge e1 e2) = do
    (t1, e1') <- verExpr tfun tloc e1
    (t2, e2') <- verExpr tfun tloc e2

    if t1 == TString || t2 == TString then do
        erroFatal "Nao e possivel fazer comparacao de >, <, >=, <= com Strings."
        return (TVoid, Rge e1' e2')
    else if t1 == TDouble && t2 == TInt then
        return (TInt, Rge e1' (IntDouble e2'))
    else if t1 == t2 then
        return (TInt, Rge e1' e2')
    else if t1 == TInt && t2 == TDouble then
        return (TInt, Rge (IntDouble e1') e2')
    else do
        erroFatal ("Os tipos das expressoes utilizadas sao incompativeis: " ++ show t1 ++ " " ++ show t2)
        return (TVoid, Rge e1' e2') 

-- ==========================================================================
-- EXPRESSOES LÓGICAS (verExprL)
-- ==========================================================================

verExprL :: TFun -> TLoc -> ExprL -> Analise (Tipo, ExprL)
verExprL tfun tloc (Rel r) = do
    (t, r') <- verExprR tfun tloc r
    return (t, Rel r')

verExprL tfun tloc (And e1 e2) = do
    (t1, e1') <- verExprL tfun tloc e1
    (t2, e2') <- verExprL tfun tloc e2
    if t1 == TInt && t2 == TInt then
        return (TInt, And e1' e2')
    else do
        erroFatal ("Tipos incompativeis para AND (&&). Esperado Int. Recebido: " ++ show t1 ++ " e " ++ show t2)
        return (TVoid, And e1' e2')

verExprL tfun tloc (Or e1 e2) = do
    (t1, e1') <- verExprL tfun tloc e1
    (t2, e2') <- verExprL tfun tloc e2
    if t1 == TInt && t2 == TInt then
        return (TInt, Or e1' e2')
    else do
        erroFatal ("Tipos incompativeis para OR (||). Esperado Int. Recebido: " ++ show t1 ++ " e " ++ show t2)
        return (TVoid, Or e1' e2')

verExprL tfun tloc (Not e1) = do
    (t1, e1') <- verExprL tfun tloc e1
    if t1 == TInt then
        return (TInt, Not e1')
    else do
        erroFatal ("Tipo incompativel para NOT (1). Esperado Int. Recebido: " ++ show t1)
        return (TVoid, Not e1')

-- ==========================================================================
-- VERIFICACAO DE COMANDOS
-- ==========================================================================

verBloco :: TFun -> TLoc -> Tipo -> Bloco -> Analise Bloco
verBloco _ _ _ [] = return []
verBloco tfun tloc tipoRet (c:cs) = do
    cmd' <- verCmd tfun tloc tipoRet c
    bloco' <- verBloco tfun tloc tipoRet cs
    return (cmd' : bloco')

verCmd :: TFun -> TLoc -> Tipo -> Comando -> Analise Comando

verCmd tfun tloc _ (Atrib id expr) = do
    tipoVar <- lookupVar tloc id
    (tipoExpr, expr') <- verExpr tfun tloc expr
    if tipoVar == TDouble && tipoExpr == TInt then 
        return (Atrib id (IntDouble expr'))
    else if tipoVar == TInt && tipoExpr == TDouble then do
        aviso ("Tentou atribuir uma expressao do tipo TDouble em uma variavel do tipo TInt, convertendo o tipo do valor da expressao para TInt...")
        return (Atrib id (DoubleInt expr'))
    else if tipoVar == tipoExpr then 
        return (Atrib id expr')
    else do 
        erroFatal "Tentativa de atribuicao de variaveis com tipos conflitantes."
        return (Atrib id expr')

verCmd tfun tloc tipoRet (If expr blocoThen blocoElse) = do 
    (tipoCond, expr') <- verExprL tfun tloc expr
    bl1 <- verBloco tfun tloc tipoRet blocoThen
    bl2 <- verBloco tfun tloc tipoRet blocoElse
    if tipoCond /= TInt then do
        erroFatal ("Esperava expressao com tipo de valor logico (TInt) no IF. Tipo recebido: " ++ show tipoCond)
        return (If expr' bl1 bl2)
    else
        return (If expr' bl1 bl2)

verCmd tfun tloc tipoRet (While expr bloco) = do
    (tipoCond, expr') <- verExprL tfun tloc expr
    blocoCorrigido <- verBloco tfun tloc tipoRet bloco
    if tipoCond /= TInt then do
        erroFatal ("Esperava expressao com tipo de valor logico (TInt) no WHILE. Tipo recebido: " ++ show tipoCond)
        return (While expr' blocoCorrigido)
    else 
        return (While expr' blocoCorrigido)

verCmd _ tloc _ (Leitura id) = do
    _ <- lookupVar tloc id -- Só verifica se a variável existe
    return (Leitura id)

verCmd tfun tloc _ (Proc id args) = do
    (tiposEsperados, tipoRetorno) <- lookupFun tfun id
    if length args /= length tiposEsperados then do
        erroFatal ("Numero incorreto de argumentos para '" ++ id ++ "'. Esperado: " ++ show (length tiposEsperados) ++ ". Recebido: " ++ show (length args))
        return (Proc id args)
    else do
        argsCorrigidos <- verificarArgs tfun tloc tiposEsperados args
        return (Proc id argsCorrigidos)

verCmd tfun tloc tipoRetFuncao (Ret maybeExpr) = do
    case maybeExpr of
        Nothing -> 
            if tipoRetFuncao == TVoid then
                return (Ret Nothing)
            else do
                erroFatal "Funcao nao-void nao pode ter retorno vazio."
                return (Ret Nothing)
        Just expr -> do
            (tipoExpr, expr') <- verExpr tfun tloc expr
            if tipoRetFuncao == TVoid then do
                erroFatal "Funcao void nao pode retornar valor."
                return (Ret (Just expr'))
            else if tipoRetFuncao == tipoExpr then
                return (Ret (Just expr'))
            else if tipoRetFuncao == TDouble && tipoExpr == TInt then
                return (Ret (Just (IntDouble expr')))
            else if tipoRetFuncao == TInt && tipoExpr == TDouble then do
                aviso "Retorno de Double para funcao Int. Perda de precisao."
                return (Ret (Just (DoubleInt expr')))
            else do
                erroFatal ("Tipo de retorno invalido. Esperado: " ++ show tipoRetFuncao ++ ". Recebido: " ++ show tipoExpr)
                return (Ret (Just expr'))

verCmd tfun tloc _ (Imp expr) = do
    (_, exprCorr) <- verExpr tfun tloc expr
    return (Imp exprCorr)

-- ==========================================================================
-- VERIFICACAO DE ARGUMENTOS
-- ==========================================================================

verificarArgs :: TFun -> TLoc -> [Tipo] -> [Expr] -> Analise [Expr]
verificarArgs _ _ [] [] = return []
verificarArgs _ _ [] _  = do 
    return [] 
verificarArgs _ _ _ []  = return []
verificarArgs tfun tloc (tEsperado:ts) (e:es) = do
    (tAtual, eCorr) <- verExpr tfun tloc e
    restoCorr <- verificarArgs tfun tloc ts es
    if tEsperado == tAtual then
        return (eCorr : restoCorr)
    else if tEsperado == TDouble && tAtual == TInt then
        return (IntDouble eCorr : restoCorr)
    else if tEsperado == TInt && tAtual == TDouble then do
        aviso "Argumento Double passado para parametro Int. Perda de precisao."
        return (DoubleInt eCorr : restoCorr)
    else do
        erroFatal ("Erro de Tipo no Argumento. Esperava " ++ show tEsperado ++ " mas recebeu " ++ show tAtual)
        return (eCorr : restoCorr)

-- ==========================================================================
-- HELPERS para ATRIBUIR INDICES às VARS (leva em conta doubles ocupando 2 slots)
-- ==========================================================================

-- recebe lista de Var (nome :#: (Tipo, _)) e índice inicial; devolve lista com índices e próximo índice livre
assignIndicesVars :: [Var] -> Int -> ([Var], Int)
assignIndicesVars [] i = ([], i)
assignIndicesVars ((nome :#: (t, _)) : vs) i =
    let idx = i
        inc = case t of
                TDouble -> 2
                _       -> 1
        (rest, next) = assignIndicesVars vs (i + inc)
    in ((nome :#: (t, idx)) : rest, next)

-- transforma Var -> (nome, tipo) para TLoc
varsToTLoc :: [Var] -> TLoc
varsToTLoc vars = map (\(nome :#: (tipo, _)) -> (nome, tipo)) vars

-- extrai tipos de Vars
getTipoVar :: Var -> Tipo
getTipoVar (_ :#: (t, _)) = t

-- extrai assinatura em formato (nome, ([Tipo], Tipo))
getAssinatura :: Funcao -> (String, ([Tipo], Tipo))
getAssinatura (nome :->: (params, ret)) = (nome, (map getTipoVar params, ret))

-- pega parametros (Vars) de uma assinatura (com índices já atribuídos)
getParametros :: [Funcao] -> String -> [Var]
getParametros [] _ = []
getParametros ((n :->: (params, _)):xs) nome
    | n == nome = params
    | otherwise = getParametros xs nome

-- ==========================================================================
-- VERIFICAÇÃO DE FUNÇÕES E PROGRAMA (agora atribuindo índices)
-- ==========================================================================

-- atribui índices aos parâmetros nas assinaturas (cada função: parâmetros recebem índices a partir de 0)
assignIndicesToSignatures :: [Funcao] -> [Funcao]
assignIndicesToSignatures [] = []
assignIndicesToSignatures ((n :->: (params, ret)) : xs) =
    let (paramsWithIdx, _) = assignIndicesVars params 0
    in (n :->: (paramsWithIdx, ret)) : assignIndicesToSignatures xs

-- verifica implementações: usa assinaturas com índices para posicionar parâmetros e atribuir índices aos locais
verFuncoes :: [Funcao] -> TFun -> [(Id, [Var], Bloco)] -> Analise [(Id, [Var], Bloco)]
verFuncoes _ _ [] = return []
verFuncoes assinaturas tfun ((nome, locais, bloco):restante) = do
    (_, tipoRetorno) <- lookupFun tfun nome

    -- parâmetos já com índices em 'assinaturas'
    let params = getParametros assinaturas nome
    -- atribui índices aos locais *após* os parâmetros (respeitando doubles)
    let maxParamSlot = case params of
            [] -> -1
            ps -> maximum $ map (\(_ :#: (t, idx)) -> if t == TDouble then idx + 1 else idx) ps
    let startLoc = maxParamSlot + 1
    let (locaisWithIdx, _) = assignIndicesVars locais startLoc

    -- monta tabela completa (locais têm prioridade)
    let tlocCompleta = varsToTLoc (locaisWithIdx ++ params)

    -- verifica bloco com essa tabela
    blocoCorrigido <- verBloco tfun tlocCompleta tipoRetorno bloco

    restoCorrigido <- verFuncoes assinaturas tfun restante

    return ((nome, locaisWithIdx, blocoCorrigido) : restoCorrigido)

-- verProg: atribui índices nas assinaturas (parâmetros), verifica funções (locais recebem índices após parâmetros),
-- atribui índices no main (varsMain a partir de 0) e verifica main
verProg :: Programa -> Analise Programa
verProg (Prog assinaturas implementacoes varsMain blocoMain) = do

    -- 1. Atribui índices nas assinaturas (parâmetros)
    let assinaturasIdx = assignIndicesToSignatures assinaturas

    -- 2. Constroi TFun (tipos) a partir de assinaturas com índices (getAssinatura ignora índices)
    let tfun = map getAssinatura assinaturasIdx

    -- 3. Verifica implementacoes (atribui índices aos locais)
    implCorrigidas <- verFuncoes assinaturasIdx tfun implementacoes

    -- 4. Atribui índices às variáveis do main (a partir de 0)
    let (varsMainWithIdx, _) = assignIndicesVars varsMain 0

    -- 5. Verifica o Bloco Principal usando a tabela do main
    let tlocMain = varsToTLoc varsMainWithIdx
    blocoMainCorrigido <- verBloco tfun tlocMain TVoid blocoMain

    -- 6. Retorna a AST com assinaturas e implementacoes atualizadas (com índices)
    return (Prog assinaturasIdx implCorrigidas varsMainWithIdx blocoMainCorrigido)
