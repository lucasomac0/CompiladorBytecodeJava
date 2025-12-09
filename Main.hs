module Main where

import AST
import Token
import qualified Lex as L
import qualified Parser as P
import Semantico
import Gerador  -- IMPORTANTE!

-- Função para lidar com o resultado semântico
tratarResultado :: Analise Programa -> IO (Maybe Programa)
tratarResultado (Result (teveErro, logMensagens, arvoreFinal)) = do
    putStrLn "\n=========================================="
    putStrLn "RELATORIO DE ANALISE SEMANTICA"
    putStrLn "=========================================="

    -- imprime os avisos/erros
    if null logMensagens
        then putStrLn "Nenhum aviso ou erro detectado."
        else putStrLn logMensagens

    putStrLn "------------------------------------------"

    if teveErro then do
        putStrLn "STATUS: FALHA"
        putStrLn "O programa contem erros semanticos graves. A compilacao foi abortada."
        return Nothing
    else do
        putStrLn "STATUS: SUCESSO"
        putStrLn "\n--- ARVORE SINTATICA DECORADA ---\n"
        print arvoreFinal
        return (Just arvoreFinal)

main :: IO ()
main = do
    -- lê o arquivo-fonte do compilador
    codigoFonte <- readFile "teste.j--"

    -- analisador lexico
    let tokens = L.alexScanTokens codigoFonte

    -- analisador sintatico
    let astBruta = P.calc tokens

    -- analisador semantico
    analise <- tratarResultado (verProg astBruta)

    case analise of
        Nothing -> return ()
        Just arvoreDecorada -> do
            let codigoJasmin = gerarCodigo "Programa" arvoreDecorada

            writeFile "Programa.j" codigoJasmin

            putStrLn "\n------------------------------------------"
            putStrLn "Arquivo Programa.j gerado com sucesso!"
            putStrLn "Agora execute:"
            putStrLn "    java -jar jasmin.jar Programa.j"
            putStrLn "    java Programa"
            putStrLn "------------------------------------------"
