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
--  return a = Result (False, "", a)
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
-- PARTE 3: Funções
-- ==========================================================================

-- funcao pra ver se a variavel existe, retorna o tipo se existir 
lookupVar :: TLoc -> String -> Analise Tipo

lookupVar [] nome = do  
    erroFatal ("Variavel" ++ nome ++ "nao encontrada")
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
        return (TVoid, Add e1Corrigida e2Corrigida)
    -- caso onde ambos tenham o mesmo tipo
    else if t1 == t2 then
        return (t1, Add e1Corrigida e2Corrigida)
    else do
        erroFatal ("Tipos incompativeis.")
        return (TVoid, Add e1Corrigida e2Corrigida)

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
        return (TVoid, Sub e1Corrigida e2Corrigida)
    -- caso onde ambos tenham o mesmo tipo
    else if t1 == t2 then
        return (t1, Sub e1Corrigida e2Corrigida)
    else do
        erroFatal "Tipos incompativeis"
        return (TVoid, Sub e1Corrigida e2Corrigida)

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
        return (TVoid, Mul e1Corrigida e2Corrigida)
    else if t1 == t2 then
        return (t1, Mul e1Corrigida e2Corrigida)
    else do
        erroFatal "Tipos incompativeis"
        return (TVoid, Mul e1Corrigida e2Corrigida)

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
        return (TVoid, Div e1Corrigida e2Corrigida)
    else if t1 == t2 then
        return (t1, Div e1Corrigida e2Corrigida)
    else do
        erroFatal "Tipos incompativeis"
        return (TVoid, Div e1Corrigida e2Corrigida)

-- EXPRESSOES RELACIONAIS

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
        return (TVoid, Req e1' e2')

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
        return (TVoid, Rdif e1' e2')

-- menor que -
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

-- maior que -
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

-- menor igual -
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

-- maior igual -
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

-- EXPRESSOES LÓGICAS

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
    else do
        erroFatal ("Tipos incompativeis para AND (&&). Esperado Int. Recebido: " ++ show t1 ++ " e " ++ show t2)
        return (TVoid, And e1' e2')

-- or
verExprL tfun tloc (Or e1 e2) = do

    (t1, e1') <- verExprL tfun tloc e1
    (t2, e2') <- verExprL tfun tloc e2

    if t1 == TInt && t2 == TInt then
        return (TInt, Or e1' e2')
    else do
        erroFatal ("Tipos incompativeis para OR (||). Esperado Int. Recebido: " ++ show t1 ++ " e " ++ show t2)
        return (TVoid, Or e1' e2')

-- not
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

-- verificador de bloco, o tipo é o tipo de retorno da função para saber se o tipo de retorno bate com o tipo declarado na funcao
verBloco :: TFun -> TLoc -> Tipo -> Bloco -> Analise Bloco

-- caso base
verBloco _ _ _ [] = return []

-- chama o verificador de comando recursivamente
verBloco tfun tloc tipoRet (c:cs) = do
    cmd' <- verCmd tfun tloc tipoRet c
    bloco' <- verBloco tfun tloc tipoRet cs
    return (cmd' : bloco')


-- verificador de comando
verCmd :: TFun -> TLoc -> Tipo -> Comando -> Analise Comando

-- atribuicao
verCmd tfun tloc _ (Atrib id expr) = do
    -- ve se a variavel existe e extrai o tipo dela
    tipoVar <- lookupVar tloc id
    -- verifica a expressao
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

-- if
verCmd tfun tloc tipoRet (If expr blocoThen blocoElse) = do 

    -- verifica a condição (Deve ser Lógica -> TInt)
    (tipoCond, expr') <- verExprL tfun tloc expr

    -- verifica os blocos
    -- tem q passar tipoRet (da função), NÃO tipoCond
    bl1 <- verBloco tfun tloc tipoRet blocoThen
    bl2 <- verBloco tfun tloc tipoRet blocoElse
    
    if tipoCond /= TInt then do
        erroFatal ("Esperava expressao com tipo de valor logico (TInt) no IF. Tipo recebido: " ++ show tipoCond)
        return (If expr' bl1 bl2)
    else
        return (If expr' bl1 bl2)

-- while
verCmd tfun tloc tipoRet (While expr bloco) = do

    (tipoCond, expr') <- verExprL tfun tloc expr

    -- tem q passar tipoRet e qualquer lista 'bloco'
    blocoCorrigido <- verBloco tfun tloc tipoRet bloco

    if tipoCond /= TInt then do
        erroFatal ("Esperava expressao com tipo de valor logico (TInt) no WHILE. Tipo recebido: " ++ show tipoCond)
        return (While expr' blocoCorrigido)
    else 
        return (While expr' blocoCorrigido)

-- read()
verCmd _ tloc _ (Leitura id) = do
    _ <- lookupVar tloc id -- Só verifica se a variável existe
    return (Leitura id)

-- procedimento (chamada de funcao void)
verCmd tfun tloc _ (Proc id args) = do
    -- busca a assinatura da função na tabela
    (tiposEsperados, tipoRetorno) <- lookupFun tfun id
    
    -- verifica se número de argumentos bate
    if length args /= length tiposEsperados then do
        erroFatal ("Numero incorreto de argumentos para '" ++ id ++ "'. Esperado: " ++ show (length tiposEsperados) ++ ". Recebido: " ++ show (length args))
        return (Proc id args)
    else do
        -- verifica cada argumento 
        argsCorrigidos <- verificarArgs tfun tloc tiposEsperados args
        return (Proc id argsCorrigidos)

-- return
verCmd tfun tloc tipoRetFuncao (Ret maybeExpr) = do
    case maybeExpr of
        -- caso 'return;' (Vazio)
        Nothing -> 
            if tipoRetFuncao == TVoid then
                return (Ret Nothing)
            else do
                erroFatal "Funcao nao-void nao pode ter retorno vazio."
                return (Ret Nothing)

        -- caso 'return expr;' (Com valor)
        Just expr -> do
            (tipoExpr, expr') <- verExpr tfun tloc expr
            
            if tipoRetFuncao == TVoid then do
                erroFatal "Funcao void nao pode retornar valor."
                return (Ret (Just expr'))
            
            else if tipoRetFuncao == tipoExpr then
                return (Ret (Just expr'))
            
            -- int -> double 
            else if tipoRetFuncao == TDouble && tipoExpr == TInt then
                return (Ret (Just (IntDouble expr')))
            
            -- double -> int (Aviso)
            else if tipoRetFuncao == TInt && tipoExpr == TDouble then do
                aviso "Retorno de Double para funcao Int. Perda de precisao."
                return (Ret (Just (DoubleInt expr')))
            
            -- Erro de tipo
            else do
                erroFatal ("Tipo de retorno invalido. Esperado: " ++ show tipoRetFuncao ++ ". Recebido: " ++ show tipoExpr)
                return (Ret (Just expr'))

-- print
verCmd tfun tloc _ (Imp expr) = do
    (_, exprCorr) <- verExpr tfun tloc expr
    return (Imp exprCorr)

-- ================================================================
-- VERIFICACAO DE ARGUMENTOS
-- ================================================================

-- verifica lista de argumentos comparando com os tipos esperados
verificarArgs :: TFun -> TLoc -> [Tipo] -> [Expr] -> Analise [Expr]
verificarArgs _ _ [] [] = return []
verificarArgs _ _ [] _  = do 
    return [] 
verificarArgs _ _ _ []  = return []

verificarArgs tfun tloc (tEsperado:ts) (e:es) = do
    -- verifica o argumento atual
    (tAtual, eCorr) <- verExpr tfun tloc e
    
    -- verifica o restante recursivamente
    restoCorr <- verificarArgs tfun tloc ts es
    
    -- compara e insere coerção
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
-- VERIFICAÇÃO DO PROGRAMA
-- ==========================================================================

-- Helper: Extrai o tipo de uma variável (ignora o Int de endereço)
getTipoVar :: Var -> Tipo
getTipoVar (_ :#: (t, _)) = t

-- Helper: Converte uma lista de Var da AST para o formato da TLoc [(String, Tipo)]
varsToTLoc :: [Var] -> TLoc
varsToTLoc vars = map (\(nome :#: (tipo, _)) -> (nome, tipo)) vars

-- Helper: Extrai a assinatura da função para a TFun
getAssinatura :: Funcao -> (String, ([Tipo], Tipo))
getAssinatura (nome :->: (params, ret)) = (nome, (map getTipoVar params, ret))

-- Helper: Busca a lista de parametros de uma função na lista de assinaturas original
getParametros :: [Funcao] -> String -> [Var]
getParametros [] _ = []
getParametros ((n :->: (params, _)):xs) nome
    | n == nome = params
    | otherwise = getParametros xs nome

-- Verifica a lista de implementações de funções
verFuncoes :: [Funcao] -> TFun -> [(Id, [Var], Bloco)] -> Analise [(Id, [Var], Bloco)]
verFuncoes _ _ [] = return []
verFuncoes assinaturas tfun ((nome, locais, bloco):restante) = do
    
    (_, tipoRetorno) <- lookupFun tfun nome
    
    -- 1. Pegamos os parâmetros originais (que tem nome E tipo)
    let params = getParametros assinaturas nome
    
    -- 2. Convertemos para o formato da TLoc
    let tlocParams = varsToTLoc params
    
    -- 3. Convertemos as locais para o formato da TLoc
    let tlocLocais = varsToTLoc locais
    
    -- 4. Junta tudo (Locais têm prioridade sobre parâmetros, que têm prioridade sobre globais se houvesse)
    let tlocCompleta = tlocLocais ++ tlocParams
    
    -- 5. Verifica o bloco com a tabela completa
    blocoCorrigido <- verBloco tfun tlocCompleta tipoRetorno bloco
    
    -- Passa 'assinaturas' para a recursão
    restoCorrigido <- verFuncoes assinaturas tfun restante
    
    return ((nome, locais, blocoCorrigido) : restoCorrigido)

verProg :: Programa -> Analise Programa
verProg (Prog assinaturas implementacoes varsMain blocoMain) = do
    
    let tfun = map getAssinatura assinaturas
    
    implCorrigidas <- verFuncoes assinaturas tfun implementacoes
    
    -- 3. Monta a Tabela de Variáveis do Main (TLoc)
    let tlocMain = varsToTLoc varsMain
    
    -- 4. Verifica o Bloco Principal
    -- O bloco principal (Main) geralmente tem retorno TVoid ou TInt (depende da linguagem).
    blocoMainCorrigido <- verBloco tfun tlocMain TVoid blocoMain
    
    -- 5. Retorna a AST inteira reconstruída
    return (Prog assinaturas implCorrigidas varsMain blocoMainCorrigido)