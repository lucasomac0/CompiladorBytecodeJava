<h1>Descrição</h1>

Trabalho final da disciplina de Compiladores. 
Se trata de um compilador feito em Haskell que gera _bytecodes Java_ para uma linguagem definida por uma gramática fornecida pelo professor.

É separado em (até então) 4 etapas:

- Analisador Léxico: responsável por pegar um arquivo Java e devolver uma string de tokens que descrevem todos os comandos do arquivo Java.
  
- Analisador Sintático: responsável por pegar a string de tokens gerada pelo analisador léxico e transformá-la em uma Árvore Sintática Abstrata (AST).
  
- Analisador Semântico: responsável por pegar a AST do analisador sintático e devolver uma AST corrigida de acordo com as regras definidas nesta etapa. Gera mensagens de erro, conversões de tipos, advertências e é responsável pela validação de tudo o que veio da AST do analisador semântico.

- Gerador de _Bytecode_: responsável por pegar a AST corrigida do analisador semântico e printar os respectivos comando no formato Assembly de JVM.

<h1>Como Testar</h1>
Aqui já tem um arquivo teste.j--, se quiser testar com outros código recomendo colar o código neste arquivo teste pra não precisar alterar o caminho do arquivo no código Main.hs.

Com tudo na mesma pasta, você vai precisar ter o Alex, o Happy, o GHC (Glasgow Haskell Compiler) e o Jasmin. Procure como faz para instalá-los com o seu gerenciador de pacotes.

Tendo tudo baixados, rode os seguintes comandos no terminal:
```bash
alex Lex.x
happy Parser.y
ghc Main.hs Semantico.hs Parser.hs Lex.hs AST.hs Token.hs -o compilador
./compilador
java -jar jasmin.jar Programa.j
java Programa
```
Com isso você conseguirá rodar o código contido no arquivo teste.j-- desde que nele tenham apenas os tokens que são descritos pelo arquivo Token.hs. É possível que haja um problema na hora de compilar caso você não tenha o arquivo jasmin.jar na pasta do compilador. Nesse caso você vai ter que criar um novo olhando a documentação do Jasmin.
