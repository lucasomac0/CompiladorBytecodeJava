module Token where

data Token =

  -- Constantes/Literais
  CINT Int -- ;
  | CDOUBLE Double -- ;
  | LSTRING String -- ;
  
  -- Tipos de dados
  | TINT -- ;
  | TDOUBLE -- ;
  | TSTRING -- ;
  | TVOID -- ;

  -- Operações
  | ADD -- ;
  | SUB -- ;
  | MUL -- ;
  | DIV -- ;
  | ATT -- atribuição ;
  
  -- Sintaxe
  | LPAR -- ;
  | RPAR -- ;
  | LBRACKET -- abre chaves ;
  | RBRACKET -- fecha chaves ;
  | SEMICOLON -- ponto e virgula ;
  | COMMA -- virgula ;

  -- Expressões aritmétcias
  | MEQ -- menor que ;
  | MAQ -- maior que ;
  | LE -- less equal ;
  | GE -- greater equal ; 
  | IG -- igual ;
  | DIFF -- different ;

  -- Operadores Lógicos
  | AND -- ;
  | OR -- ;
  | NOT -- ;
  
  -- Palavras reservadas
  | IF -- ;
  | ELSE -- ;
  | WHILE -- ;
  | RETURN -- ;
  | FOR -- ;
  | READ -- ;
  | PRINT -- ;

  -- Identificador
  | ID String-- ;

  deriving (Eq, Show)
  
