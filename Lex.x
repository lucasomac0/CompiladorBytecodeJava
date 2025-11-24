{
module Lex where

import Token
}

%wrapper "basic"

$digit = [0-9]          -- digits
$char = [a-zA-Z] -- caracteres
@num_double = $digit+\.$digit+ -- antes tava escrito $digit+(\.digit+)?, o que indicava que o numero apos a virgula era opcional, mas se deixasse desse jeito o double ia "roubar" os valores que deveriam ser inteiros
@num_int = $digit+
@string = \"[^\"]*\"
@id = $char($char | $digit | \_)* -- começa com uma letra, seguida por zero ou mais letras, numeros ou underscores

tokens :-

<0> $white+ ;

-- Palavras reservadas (devem vir antes do identificador pois o criterio de desempate do alex é quem aparece primeiro no arquivo)
<0> "if" {\s -> IF}
<0> "else" {\s -> ELSE}
<0> "while" {\s -> WHILE}
<0> "return" {\s -> RETURN}
<0> "for" {\s -> FOR}
<0> "read" {\s -> READ}
<0> "print" {\s -> PRINT}

-- Tipos de dados 
<0> "int" {\s -> TINT}
<0> "double" {\s -> TDOUBLE}
<0> "string" {\s -> TSTRING}
<0> "void" {\s -> TVOID}    

-- Constantes/Litearias
<0> @num_double {\s -> CDOUBLE (read s)}
<0> @num_int {\s -> CINT (read s)}
<0> @string {\s -> LSTRING (drop 1(take (length s - 1) s))} -- truque pra remover as aspas de uma string ("teste" -> teste)

-- Operacoes
<0> "+" {\s -> ADD}  
<0> "-" {\s -> SUB}  
<0> "*" {\s -> MUL}  
<0> "/" {\s -> DIV}
<0> "=" {\s -> ATT}

-- Sintaxe
<0> "(" {\s -> LPAR}  
<0> ")" {\s -> RPAR}  
<0> "{" {\s -> LBRACKET}
<0> "}" {\s -> RBRACKET}
<0> ";" {\s -> SEMICOLON}
<0> "," {\s -> COMMA}

-- Expressões Aritméticas
<0> "<" {\s -> MEQ} 
<0> ">" {\s -> MAQ}
<0> "<=" {\s -> LE}
<0> ">=" {\s -> GE}
<0> "==" {\s -> IG}
<0> "/=" {\s -> DIFF}

-- Operadores Lógicos
<0> "&&" {\s -> AND}
<0> "||" {\s -> OR}
<0> "!" {\s -> NOT}

-- Identificador
<0> @id {\s -> ID s}

{
main :: IO ()
main = do
  s <- getContents  -- 
  print (alexScanTokens s)
}