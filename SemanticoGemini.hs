module Semantico where

import AST

-- =================================================================
-- TABELAS DE SÍMBOLOS
-- =================================================================

-- Tabela de Funções: Nome -> (Tipos dos Parâmetros, Tipo de Retorno)
type TFun = [(Id, ([Tipo], Tipo))]

-- Tabela de Variáveis: Nome -> Tipo
type TLoc = [(Id, Tipo)]

-- =================================================================
-- AUXILIARES
-- =================================================================

-- Busca variável no escopo
lookupVar :: TLoc -> Id -> Either String Tipo
lookupVar [] nome = Left ("Erro Semantico: Variavel nao declarada -> " ++ nome)
lookupVar ((n,t):xs) nome 
    | n == nome = Right t
    | otherwise = lookupVar xs nome

-- Busca função
lookupFun :: TFun -> Id -> Either String ([Tipo], Tipo)
lookupFun [] nome = Left ("Erro Semantico: Funcao nao declarada -> " ++ nome)
lookupFun ((n,d):xs) nome 
    | n == nome = Right d
    | otherwise = lookupFun xs nome

-- =================================================================
-- VERIFICAÇÃO DO PROGRAMA (Entrada Principal)
-- =================================================================

-- O Parser gera: Prog [Assinaturas] [Implementações] [Globais] [BlocoMain]
verProg :: Programa -> Either String Programa
verProg (Prog funcs impls globais mainBloco) = do
    -- 1. Monta tabela de funções (TFun) baseada na lista 'funcs'
    let tfun = map (\(nome :->: (vars, ret)) -> (nome, (map getTipoVar vars, ret))) funcs
    
    -- 2. Monta tabela de variáveis globais
    let tlocGlobal = map (\(nome :#: (tipo, _)) -> (nome, tipo)) globais

    -- 3. Aqui você deveria verificar cada função em 'impls' (implementações)
    -- (Vou pular essa parte por enquanto para focar no Bloco Principal e Expressões)

    -- 4. Verifica o Bloco Principal
    mainBlocoCorrigido <- verBloco tfun tlocGlobal mainBloco

    -- Retorna a AST reconstruída e corrigida (com os casts inseridos)
    return (Prog funcs impls globais mainBlocoCorrigido)

-- Função auxiliar para extrair tipo de uma Var (Id :#: (Tipo, Int))
getTipoVar :: Var -> Tipo
getTipoVar (_ :#: (t, _)) = t

-- =================================================================
-- VERIFICAÇÃO DE BLOCOS E COMANDOS
-- =================================================================

verBloco :: TFun -> TLoc -> Bloco -> Either String Bloco
verBloco _ _ [] = return []
verBloco tfun tloc (c:cs) = do
    cmdCorrigido <- verCmd tfun tloc c
    restoCorrigido <- verBloco tfun tloc cs
    return (cmdCorrigido : restoCorrigido)

verCmd :: TFun -> TLoc -> Comando -> Either String Comando
verCmd tfun tloc (Atrib id expr) = do
    tipoVar <- lookupVar tloc id
    (tipoExpr, exprCorrigida) <- verExpr tfun tloc expr
    
    if tipoVar == tipoExpr then
        return (Atrib id exprCorrigida)
    else if tipoVar == TDouble && tipoExpr == TInt then
        -- AQUI ACONTECE A MÁGICA: Se a variável é Double e vem um Int, converte.
        return (Atrib id (IntDouble exprCorrigida))
    else
        Left $ "Erro de Atribuicao: Variavel '" ++ id ++ "' espera " ++ show tipoVar ++ " mas recebeu " ++ show tipoExpr

verCmd tfun tloc (Imp expr) = do
    (_, expr') <- verExpr tfun tloc expr
    return (Imp expr')

verCmd tfun tloc (Proc nome args) = do
    (tiposParams, tipoRet) <- lookupFun tfun nome
    
    if length args /= length tiposParams 
    then Left $ "Procedimento '" ++ nome ++ "' espera " ++ show (length tiposParams) ++ " argumentos."
    else do
        argsCorrigidos <- verificarArgs tfun tloc tiposParams args
        return (Proc nome argsCorrigidos)

-- (Outros comandos como If, While, precisam verificar ExprL, deixei simplificado)
verCmd _ _ cmd = return cmd 

-- =================================================================
-- VERIFICAÇÃO DE EXPRESSÕES ARITMÉTICAS (ExprA)
-- =================================================================
-- Retorna: (Tipo Resultante, AST Modificada)

verExpr :: TFun -> TLoc -> Expr -> Either String (Tipo, Expr)

-- Literais
verExpr _ _ (Const (CInt n))    = return (TInt, Const (CInt n))
verExpr _ _ (Const (CDouble n)) = return (TDouble, Const (CDouble n))
verExpr _ _ (Lit s)             = return (TString, Lit s)

-- Variáveis
verExpr _ tloc (IdVar nome) = do
    t <- lookupVar tloc nome
    return (t, IdVar nome)

-- Operações Binárias (Exemplo com ADD, deve replicar para SUB, MUL, DIV)
verExpr tfun tloc (Add e1 e2) = do
    (t1, e1') <- verExpr tfun tloc e1
    (t2, e2') <- verExpr tfun tloc e2
    
    case (t1, t2) of
        (TInt, TInt)       -> return (TInt, Add e1' e2')
        (TDouble, TDouble) -> return (TDouble, Add e1' e2')
        
        -- REGRA DE COERÇÃO (Int + Double = Double)
        (TInt, TDouble)    -> return (TDouble, Add (IntDouble e1') e2')
        (TDouble, TInt)    -> return (TDouble, Add e1' (IntDouble e2'))
        
        (TString, _)       -> Left "Erro: Nao pode somar String em expressao aritmetica"
        (_, TString)       -> Left "Erro: Nao pode somar String em expressao aritmetica"
        _                  -> Left "Tipos incompativeis na soma"

-- Chamada de Função
-- Dentro de verExpr...

verExpr tfun tloc (Chamada nome args) = do
    -- Busca a assinatura da função
    (tiposParams, tipoRet) <- lookupFun tfun nome
    
    if length args /= length tiposParams 
    then Left $ "Funcao '" ++ nome ++ "' espera " ++ show (length tiposParams) ++ " argumentos, mas recebeu " ++ show (length args)
    else do
        -- CHAMA A NOVA FUNÇÃO AQUI:
        argsCorrigidos <- verificarArgs tfun tloc tiposParams args
        return (tipoRet, Chamada nome argsCorrigidos)

-- Caso default para coisas não implementadas ainda
verExpr _ _ e = Left ("Expressao nao suportada ainda: " ++ show e)

-- Verifica lista de argumentos comparando com os tipos esperados
verificarArgs :: TFun -> TLoc -> [Tipo] -> [Expr] -> Either String [Expr]
verificarArgs tfun tloc [] [] = return []
verificarArgs tfun tloc (tEsperado:ts) (e:es) = do
    -- 1. Verifica a expressão do argumento atual para saber o tipo dela
    (tAtual, eCorrigida) <- verExpr tfun tloc e
    
    -- 2. Verifica o restante dos argumentos recursivamente
    restoCorrigido <- verificarArgs tfun tloc ts es
    
    -- 3. Compara Tipo Esperado vs Tipo Atual e insere coerção
    if tEsperado == tAtual then
        return (eCorrigida : restoCorrigido)
        
    else if tEsperado == TDouble && tAtual == TInt then
        -- A função espera Double, mas veio Int -> Converte
        return (IntDouble eCorrigida : restoCorrigido)
        
    else if tEsperado == TInt && tAtual == TDouble then
        -- A função espera Int, mas veio Double -> Converte (com warning implícito)
        return (DoubleInt eCorrigida : restoCorrigido)
        
    else
        Left $ "Erro de Tipo no Argumento: Esperava " ++ show tEsperado ++ " mas recebeu " ++ show tAtual

verificarArgs _ _ _ _ = Left "Numero incorreto de argumentos (Erro interno na recursao)"