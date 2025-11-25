module Main where

import AST
import Token
import qualified Lex as L
import qualified Parser as P
import Semantico -- Importa a função principal e a Mônada

-- Função para desembrulhar o tipo Result definido no Semantico.hs
-- Como o Result foi exportado como Analise(..), podemos acessar seus campos internos aqui.
-- A estrutura é: data Result a = Result (Bool, String, a)
tratarResultado :: Analise Programa -> IO ()
tratarResultado (Result (teveErro, logMensagens, arvoreFinal)) = do
    putStrLn "\n=========================================="
    putStrLn "RELATORIO DE ANALISE SEMANTICA"
    putStrLn "=========================================="
    
    -- imprime o log de mensagens (Erros e Avisos acumulados)
    if null logMensagens 
        then putStrLn "Nenhum aviso ou erro detectado."
        else putStrLn logMensagens
        
    putStrLn "------------------------------------------"
    
    -- verificador de erro
    if teveErro then do
        putStrLn "STATUS: FALHA"
        putStrLn "O programa contem erros semanticos graves. A compilacao foi abortada."
    else do
        putStrLn "STATUS: SUCESSO"
        putStrLn "\n--- ARVORE SINTATICA DECORADA ---\n"
        print arvoreFinal

main :: IO ()
main = do
    -- leitura do arquivo
    arquivo <- readFile "teste.j--"
    
    -- analisador lexico
    let tokens = L.alexScanTokens arquivo
    -- putStrLn ("Tokens gerados: " ++ show tokens) -- serve p debug
    
    -- 3. analisador sintatico
    let astBruta = P.calc tokens
    -- putStrLn ("\nAST Bruta (Antes do Semantico): " ++ show astBruta) -- serve p debug
    
    -- 4. analisador semantico
    let resultado = verProg astBruta
    
    -- 5. saida do analisador semantico
    tratarResultado resultado