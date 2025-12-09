module Gerador where

import AST
import Control.Monad.State
import Data.List (lookup, foldl')
import Data.Maybe (fromMaybe)

-- ============================================
-- Mônada e tipos auxiliares
-- ============================================

type Gerador = State Int
type TFun = [(String, ([Tipo], Tipo))]
-- agora TabIndices guarda (Tipo, Índice)
type TabIndices = [(String, (Tipo, Int))]

novoLabel :: Gerador String
novoLabel = do
  n <- get
  put (n + 1)
  return ("L" ++ show n)

-- ============================================
-- Helpers Jasmin (strings com \t e \n)
-- ============================================

genOp :: Tipo -> String -> String
genOp TInt op    = "\ti" ++ op ++ "\n"
genOp TDouble op = "\td" ++ op ++ "\n"
genOp _ _        = error "genOp: tipo inválido"

genLoad :: Tipo -> Int -> String
genLoad TInt idx    = "\tiload " ++ show idx ++ "\n"
genLoad TDouble idx = "\tdload " ++ show idx ++ "\n"
genLoad TString idx = "\taload " ++ show idx ++ "\n"
genLoad _ _         = error "genLoad: tipo inválido"

genStore :: Tipo -> Int -> String
genStore TInt idx    = "\tistore " ++ show idx ++ "\n"
genStore TDouble idx = "\tdstore " ++ show idx ++ "\n"
genStore TString idx = "\tastore " ++ show idx ++ "\n"
genStore _ _         = error "genStore: tipo inválido"

genAssinaturaTipo :: Tipo -> String
genAssinaturaTipo TInt    = "I"
genAssinaturaTipo TDouble = "D"
genAssinaturaTipo TString = "Ljava/lang/String;"
genAssinaturaTipo TVoid   = "V"

genCab :: String -> Gerador String
genCab nome = return $ concat
  [ ".class public ", nome, "\n"
  , ".super java/lang/Object\n\n"
  , ".method public <init>()V\n"
  , "\taload_0\n"
  , "\tinvokenonvirtual java/lang/Object/<init>()V\n"
  , "\treturn\n"
  , ".end method\n\n"
  ]

genMainCab :: Int -> Int -> Gerador String
genMainCab s l = return $ concat
  [ ".method public static main([Ljava/lang/String;)V\n"
  , "\t.limit stack ", show s, "\n"
  , "\t.limit locals ", show l, "\n\n"
  ]

-- ============================================
-- Comparações
-- ============================================

genRel :: Tipo -> Tipo -> String -> String -> String
genRel TInt TInt lbl op = "\tif_icmp" ++ op ++ " " ++ lbl ++ "\n"
genRel TDouble TDouble lbl op = "\tdcmpg\n\tif" ++ op ++ " " ++ lbl ++ "\n"
genRel TString TString lbl op =
  "\tinvokevirtual java/lang/String/equals(Ljava/lang/Object;)Z\n"
  ++ if op == "eq" then "\tifne " ++ lbl ++ "\n" else "\tifeq " ++ lbl ++ "\n"
genRel _ _ _ _ = error "genRel: tipos incompatíveis"

-- ============================================
-- Geração de Expressões (única definição)
-- ============================================

genExpr :: String -> TabIndices -> TFun -> Expr -> Gerador (Tipo, String)

-- constantes
genExpr _ _ _ (Const (CInt n))    = return (TInt,    "\tldc " ++ show n ++ "\n")
genExpr _ _ _ (Const (CDouble d)) = return (TDouble, "\tldc2_w " ++ show d ++ "\n")
genExpr _ _ _ (Const (CString s)) = return (TString, "\tldc " ++ show s ++ "\n")

-- variável: agora retorna o tipo guardado em TabIndices
genExpr _ tab _ (IdVar nome) =
  case lookup nome tab of
    Just (tipo, idx) -> return (tipo, genLoad tipo idx)
    Nothing -> error $ "genExpr(IdVar): variável não encontrada: " ++ nome

-- literal string
genExpr _ _ _ (Lit s) = return (TString, "\tldc " ++ show s ++ "\n")

-- coerções controladas (semântico já inseriu onde necessário)
genExpr cls tab fun (IntDouble e) = do
  (t, c) <- genExpr cls tab fun e
  if t == TInt then return (TDouble, c ++ "\ti2d\n")
               else error "genExpr(IntDouble): expressão não é TInt"
genExpr cls tab fun (DoubleInt e) = do
  (t, c) <- genExpr cls tab fun e
  if t == TDouble then return (TInt, c ++ "\td2i\n")
                  else error "genExpr(DoubleInt): expressão não é TDouble"

-- unário neg
genExpr cls tab fun (Neg e) = do
  (t, c) <- genExpr cls tab fun e
  case t of
    TInt    -> return (TInt, c ++ "\tineg\n")
    TDouble -> return (TDouble, c ++ "\tdneg\n")
    _       -> error "genExpr(Neg): tipo não-numérico"

-- binárias
genExpr cls tab fun (Add a b) = genBin cls tab fun a b "add"
genExpr cls tab fun (Sub a b) = genBin cls tab fun a b "sub"
genExpr cls tab fun (Mul a b) = genBin cls tab fun a b "mul"
genExpr cls tab fun (Div a b) = genBin cls tab fun a b "div"

genExpr cls tab fun (Chamada nome args) = do
  (params, ret) <- case lookup nome fun of
    Just s -> return s
    Nothing -> error $ "genExpr(Chamada): função não encontrada: " ++ nome
  argCodes <- mapM (genExpr cls tab fun) args
  let codeArgs = concatMap snd argCodes
  let paramsSig = concatMap genAssinaturaTipo params
  let signature = "(" ++ paramsSig ++ ")" ++ genAssinaturaTipo ret
  return (ret, codeArgs ++ "\tinvokestatic " ++ cls ++ "/" ++ nome ++ signature ++ "\n")

genBin :: String -> TabIndices -> TFun -> Expr -> Expr -> String -> Gerador (Tipo, String)
genBin cls tab fun e1 e2 op = do
  (t1, c1) <- genExpr cls tab fun e1
  (t2, c2) <- genExpr cls tab fun e2
  case (t1, t2) of
    (TInt, TInt)      -> return (TInt,    c1 ++ c2 ++ genOp TInt op)
    (TDouble, TDouble)-> return (TDouble, c1 ++ c2 ++ genOp TDouble op)
    (TInt, TDouble)   -> return (TDouble, c1 ++ "\ti2d\n" ++ c2 ++ genOp TDouble op)
    (TDouble, TInt)   -> return (TDouble, c1 ++ c2 ++ "\ti2d\n" ++ genOp TDouble op)
    _ -> error "genBin: tipos incompatíveis"

-- ============================================
-- Expressões relacionais (ExprR) e lógicas (ExprL)
-- ============================================

genExprR :: String -> TabIndices -> TFun -> String -> ExprR -> Gerador String
genExprR cls tab fun lbl (Req a b)  = genRelOp cls tab fun lbl "eq" a b
genExprR cls tab fun lbl (Rdif a b) = genRelOp cls tab fun lbl "ne" a b
genExprR cls tab fun lbl (Rlt a b)  = genRelOp cls tab fun lbl "lt" a b
genExprR cls tab fun lbl (Rgt a b)  = genRelOp cls tab fun lbl "gt" a b
genExprR cls tab fun lbl (Rle a b)  = genRelOp cls tab fun lbl "le" a b
genExprR cls tab fun lbl (Rge a b)  = genRelOp cls tab fun lbl "ge" a b

genRelOp :: String -> TabIndices -> TFun -> String -> String -> Expr -> Expr -> Gerador String
genRelOp cls tab fun lbl op e1 e2 = do
  (t1, c1) <- genExpr cls tab fun e1
  (t2, c2) <- genExpr cls tab fun e2
  return (c1 ++ c2 ++ genRel t1 t2 lbl op)

genExprL :: String -> TabIndices -> TFun -> String -> String -> ExprL -> Gerador String
genExprL cls tab fun lT lF (And a b) = do
  mid <- novoLabel
  aC <- genExprL cls tab fun mid lF a
  bC <- genExprL cls tab fun lT lF b
  return (aC ++ mid ++ ":\n" ++ bC)
genExprL cls tab fun lT lF (Or a b) = do
  mid <- novoLabel
  aC <- genExprL cls tab fun lT mid a
  bC <- genExprL cls tab fun lT lF b
  return (aC ++ mid ++ ":\n" ++ bC)
genExprL cls tab fun lT lF (Not e) = genExprL cls tab fun lF lT e
genExprL cls tab fun lT lF (Rel r) = do
  rC <- genExprR cls tab fun lT r
  return (rC ++ "\tgoto " ++ lF ++ "\n")

-- ============================================
-- Comandos e Blocos
-- ============================================

genBloco :: String -> TabIndices -> TFun -> Bloco -> Gerador String
genBloco _ _ _ [] = return ""
genBloco cls tab fun (c:cs) = do
  c1 <- genCmd cls tab fun c
  c2 <- genBloco cls tab fun cs
  return (c1 ++ c2)

genCmd :: String -> TabIndices -> TFun -> Comando -> Gerador String

genCmd cls tab fun (Atrib nome expr) = do
  (ty, code) <- genExpr cls tab fun expr
  case lookup nome tab of
    Just (_, idx) -> return (code ++ genStore ty idx)
    Nothing -> error $ "genCmd(Atrib): variável não encontrada: " ++ nome

genCmd cls tab _ (Leitura nome) =
  case lookup nome tab of
    Just (TInt, idx) ->
      let code = concat
            [ "\tnew java/util/Scanner\n"
            , "\tdup\n"
            , "\tgetstatic java/lang/System/in Ljava/io/InputStream;\n"
            , "\tinvokespecial java/util/Scanner/<init>(Ljava/io/InputStream;)V\n"
            , "\tinvokevirtual java/util/Scanner/nextInt()I\n"
            ]
      in return (code ++ genStore TInt idx)
    Just (t, _) -> error $ "genCmd(Leitura): variável não é TInt: " ++ show t
    Nothing -> error $ "genCmd(Leitura): variável não encontrada: " ++ nome

genCmd cls tab fun (Imp expr) = do
  (t, code) <- genExpr cls tab fun expr
  return $ concat
    [ "\tgetstatic java/lang/System/out Ljava/io/PrintStream;\n"
    , code
    , "\tinvokevirtual java/io/PrintStream/println("
    , genAssinaturaTipo t
    , ")V\n"
    ]

genCmd cls tab fun (Proc nome args) = do
  (params, ret) <- case lookup nome fun of
    Just s  -> return s
    Nothing -> error $ "genCmd(Proc): função não encontrada: " ++ nome
  argCodes <- mapM (genExpr cls tab fun) args
  let codeArgs = concatMap snd argCodes
  let sigParams = concatMap genAssinaturaTipo params
  let signature = "(" ++ sigParams ++ ")" ++ genAssinaturaTipo ret
  return (codeArgs ++ "\tinvokestatic " ++ cls ++ "/" ++ nome ++ signature ++ "\n")

genCmd cls tab fun (If cond thenB elseB) = do
  lT <- novoLabel
  lF <- novoLabel
  lE <- novoLabel
  condCode <- genExprL cls tab fun lT lF cond
  thenCode <- genBloco cls tab fun thenB
  elseCode <- genBloco cls tab fun elseB
  return $ concat
    [ condCode
    , lT, ":\n"
    , thenCode
    , "\tgoto ", lE, "\n"
    , lF, ":\n"
    , elseCode
    , lE, ":\n"
    ]

genCmd cls tab fun (While cond bloco) = do
  lStart <- novoLabel
  lT <- novoLabel
  lF <- novoLabel
  condCode <- genExprL cls tab fun lT lF cond
  bodyCode <- genBloco cls tab fun bloco
  return $ concat
    [ lStart, ":\n"
    , condCode
    , lT, ":\n"
    , bodyCode
    , "\tgoto ", lStart, "\n"
    , lF, ":\n"
    ]

genCmd _ _ _ (Ret Nothing) = return "\treturn\n"
genCmd cls tab _ (Ret (Just e)) = do
  (ty, code) <- genExpr cls tab undefined e
  let instr = case ty of
        TInt -> "ireturn"
        TDouble -> "dreturn"
        TString -> "areturn"
        _ -> error "genCmd(Ret): tipo inválido"
  return (code ++ "\t" ++ instr ++ "\n")

-- ============================================
-- Funções auxiliares para transformar Vars -> TabIndices
-- ============================================

getVarsFuncao :: Funcao -> [Var]
getVarsFuncao (_ :->: (vars, _)) = vars

getTipoVar :: Var -> Tipo
getTipoVar (_ :#: (t, _)) = t

varsToTabIndices :: [Var] -> TabIndices
varsToTabIndices vars = map (\(nome :#: (t, idx)) -> (nome, (t, idx))) vars

-- calcula o slot máximo ocupado por um conjunto de variáveis
maxSlotOccupied :: TabIndices -> Int
maxSlotOccupied tab =
  foldl' (\acc (_, (t, idx)) ->
            let lastSlot = case t of
                              TDouble -> idx + 1  -- ocupa idx e idx+1
                              _       -> idx
            in max acc lastSlot
         ) 0 tab

-- ============================================
-- Geração de Funções
-- ============================================

genFuncao :: String -> TFun -> Funcao -> (Id, [Var], Bloco) -> Gerador String
genFuncao className tfun assinaturaFunc (nome, locais, bloco) = do
  (paramTypes, retType) <- case lookup nome tfun of
    Just s -> return s
    Nothing -> error $ "genFuncao: assinatura não encontrada: " ++ nome

  let paramsVars = getVarsFuncao assinaturaFunc
  let tlocLocais = varsToTabIndices locais
  let tlocParams = varsToTabIndices paramsVars
  let tlocCompleta = tlocLocais ++ tlocParams

  let maxSlot = maxSlotOccupied tlocCompleta
  -- número de locals = último slot ocupado + 1 (porque slots são 0-based)
  let numLocals = maxSlot + 1
  let numStack = 50

  let paramsSig = concatMap genAssinaturaTipo paramTypes
  let sigRet = genAssinaturaTipo retType

  corpo <- genBloco className tlocCompleta tfun bloco
  let defaultRet = if retType == TVoid then "\treturn\n" else ""

  return $ concat
    [ ".method public static ", nome, "(", paramsSig, ")", sigRet, "\n"
    , "\t.limit stack ", show numStack, "\n"
    , "\t.limit locals ", show numLocals, "\n\n"
    , corpo, defaultRet
    , ".end method\n\n"
    ]

genFuncoes :: String -> [Funcao] -> TFun -> [(Id,[Var],Bloco)] -> Gerador String
genFuncoes _ _ _ [] = return ""
genFuncoes className assin tfun (impl:rest) = do
  let nome = (\(n,_,_) -> n) impl
  let assinOrig = case lookup nome (map (\f@(n :->: _) -> (n,f)) assin) of
        Just a -> a
        Nothing -> error $ "genFuncoes: assinatura original não encontrada: " ++ nome
  c1 <- genFuncao className tfun assinOrig impl
  c2 <- genFuncoes className assin tfun rest
  return (c1 ++ c2)

-- ============================================
-- Geração do Programa Principal (main)
-- ============================================

genProg :: String -> Programa -> Gerador String
genProg className (Prog assin implementacoes varsMain blocoMain) = do
  let tfun = map getAssinatura assin

  cabClass <- genCab className
  funcoesCod <- genFuncoes className assin tfun implementacoes

  let tlocMain = varsToTabIndices varsMain
  let maxSlotMain = maxSlotOccupied tlocMain

  -- precisamos reservar o slot 0 para String[] args do main
  -- assim, o número total de locals deve ser:
  -- lastSlotIndex (considerando doubles) + 1 (para contar) + 1 (slot 0 para args) + margem
  let numLocalsMain = (maxSlotMain + 1) + 1 + 1  -- +1 para converter índice -> count, +1 args, +1 margem
  let numStackMain = 50

  cabMain <- genMainCab numStackMain numLocalsMain
  blocoMainCod <- genBloco className tlocMain tfun blocoMain

  let endMain = "\treturn\n.end method\n"

  return (cabClass ++ funcoesCod ++ cabMain ++ blocoMainCod ++ endMain)

getAssinatura :: Funcao -> (String, ([Tipo], Tipo))
getAssinatura (nome :->: (params, ret)) = (nome, (map getTipoVar params, ret))

-- ============================================
-- Entry point
-- ============================================

gerarCodigo :: String -> Programa -> String
gerarCodigo className prog = evalState (genProg className prog) 0
