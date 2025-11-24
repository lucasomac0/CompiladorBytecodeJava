{
module Parser where

import Token
import AST
import qualified Lex as L
--import Semantico (verProg)
}


%name calc
%tokentype { Token }
%error { parseError }
%token 
  '+' {ADD}
  '-' {SUB}
  '*' {MUL}
  '/' {DIV}
  '=' {ATT}
  '(' {LPAR}
  ')' {RPAR}
  ',' {COMMA}
  '{' {LBRACKET}
  '}' {RBRACKET}
  ';' {SEMICOLON}
  '<' {MEQ}
  '>' {MAQ}
  '<=' {LE}
  '>=' {GE}
  '==' {IG}
  '/=' {DIFF}
  '&&' {AND}
  '||' {OR}
  '!'  {NOT}
  'int' {TINT}
  'double' {TDOUBLE}
  'string' {TSTRING}
  'void' {TVOID}
  'if' {IF}
  'else' {ELSE}
  'while' {WHILE}
  'return' {RETURN}
  'for' {FOR}
  'read' {READ}
  'print' {PRINT}
  Int {CINT $$}
  Double {CDOUBLE $$}
  String {LSTRING $$}
  Id {ID $$}
  


%%


Programa: ListaFunc BlocoPrincipal        {Prog (map fst $1) (map (\((ident :->: (vars, tipo)), bloco) -> (ident, fst bloco, snd bloco)) $1) (fst $2) (snd $2)}
      | BlocoPrincipal                    {Prog [] [] (fst $1) (snd $1)}


ListaFunc: ListaFunc Func                 {$1++[$2]}
            | Func                        {[$1]}


Func: TipoRetorno Id '(' DeclParametros ')' BlocoPrincipal   {($2 :->: ($4, $1)), ($6)}     
      | TipoRetorno Id '(' ')' BlocoPrincipal                {($2 :->: ([], $1)), ($5)}   


TipoRetorno: Tipo                         {$1}
            | 'void'                      {TVoid}


DeclParametros: DeclParametros ',' Parametro    {$1++[$3]}
                  | Parametro                   {[$1]}


Parametro: Tipo Id                              {$2 :#: ($1,0)}


BlocoPrincipal: '{' Declaracoes ListaCmd '}'    {$2,$3}
            | '{' ListaCmd '}'                  {[],$2}


Declaracoes: Declaracoes Declaracao             {$1++$2}      
            | Declaracao                        {$1}


Declaracao: Tipo ListaId ';'              {map (\ident -> ident :#: ($1,0)) $2}                


Tipo: 'int'                               {TInt}
      | 'string'                          {TString}
      | 'double'                          {TDouble}


ListaId: ListaId ',' Id                   {$1++[$3]}                   
            | Id                          {[$1]}


Bloco: '{' ListaCmd '}'                   {$2}


ListaCmd: ListaCmd Comando                {$1++[$2]}
            | Comando                     {[$1]}                           


Comando: CmdIf                            {$1}
      | CmdWhile                          {$1}
      | CmdAtrib                          {$1} 
      | CmdEscrita                        {$1}
      | CmdLeitura                        {$1}
      | ChamadaProc                       {$1}                             
      | Retorno                           {$1}


Retorno: 'return' ExprA ';'               {Ret (Just $2)}
      | 'return' String ';'               {Ret (Just (Lit $2))}
      | 'return' ';'                      {Ret (Nothing)} 



CmdIf: 'if' '(' ExprL ')' Bloco                       {If $3 $5 []}
      | 'if' '(' ExprL ')' Bloco 'else' Bloco         {If $3 $5 $7}


CmdWhile: 'while' '(' ExprL ')' Bloco                 {While $3 $5}


CmdAtrib: Id '=' ExprA ';'                            {Atrib $1 $3}
            | Id '=' String ';'                       {Atrib $1 (Lit $3)}


CmdEscrita: 'print' '(' ExprA ')' ';'                 {Imp $3}
            | 'print' '(' String ')' ';'              {Imp (Lit $3)}


CmdLeitura: 'read' '(' Id ')' ';'                     {Leitura $3}


ChamadaProc: ChamadaFunc ';'                          {$1}


ChamadaFunc: Id '(' ListaP ')'                        {Proc $1 $3}
            | Id '(' ')'                              {Proc $1 []}


ListaP : ListaP ',' ExprA                 {$1++[$3]}
      | ListaP ',' String                 {$1++[Lit $3]}
      | ExprA                             {[$1]}
      | String                            {[Lit $1]}


ExprL: ExprL '&&' ExprL2                  {And $1 $3}
      | ExprL '||' ExprL2                 {Or $1 $3} 
      | ExprL2                            {$1}        

ExprL2: '!' ExprL3                          {Not $2}
      | ExprL3                              {$1}

ExprL3: '('  ExprL  ')'                   {$2}
      | ExprR                             {Rel $1}


ExprR: ExprA '==' ExprA                   {Req $1 $3}
      | ExprA '/=' ExprA                  {Rdif $1 $3}
      | ExprA '>' ExprA                   {Rgt $1 $3}
      | ExprA '<' ExprA                   {Rlt $1 $3}
      | ExprA '>=' ExprA                  {Rge $1 $3}
      | ExprA '<=' ExprA                  {Rle $1 $3}


ExprA: ExprA '+' ExprA2                   {Add $1 $3}
      | ExprA '-' ExprA2                  {Sub $1 $3}
      | ExprA2                            {$1}                      

ExprA2: ExprA2 '*' ExprA3                 {Mul $1 $3}
      | ExprA2 '/' ExprA3                 {Div $1 $3}
      | ExprA3                            {$1}
      | '-' ExprA3                        {Neg $2}   

ExprA3: '(' ExprA ')'                     {$2}
      | Double                            {Const (CDouble $1)}
      | Int                               {Const (CInt $1)}
      | Id                                {IdVar $1}        
      | Id '('  ')'                       {Chamada $1 []}             
      | Id '(' ListaP ')'                 {Chamada $1 $3}



-- main pra quando o semantico estiver pronto, só se liga se ta usando monada ou n (acho q n ta)

{-main = do 
    s <- readFile "teste.j--" 
    let ast = calc (L.alexScanTokens s)
    -- Agora chama a verificação semântica
    case verProg ast of
        Left erro -> putStrLn ("ERRO SEMANTICO: " ++ erro)
        Right astCorrigida -> print astCorrigida
}
-}

{
parseError :: [Token] -> a
parseError s = error ("Parse error:" ++ show s)

main :: IO ()
main = do 
    -- 1. Lê o arquivo inteiro (não apenas uma linha)
    s <- readFile "teste.j--" 
    
    -- 2. Gera os tokens
    let tokens = L.alexScanTokens s
    
    -- 3. Gera a AST (O Parser vai funcionar pois o arquivo é um Programa válido)
    let ast = calc tokens
    
    -- 4. Imprime a AST bruta (para você conferir se os nós Add, Sub, etc estão lá)
    print ast
}