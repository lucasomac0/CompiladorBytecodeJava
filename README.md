<h1>Descrição</h1>

Trabalho final da disciplina de Compiladores. 
Se trata de um compilador feito em Haskell que gera _bytecodes Java_ para uma linguagem definida por uma gramática fornecida pelo professor.

É separado em (até então) 3 etapas:

- Analisador Léxico: responsável por pegar um arquivo Java e devolver uma string de tokens que descrevem todos os comandos do arquivo Java.
  
- Analisador Sintático: responsável por pegar a string de tokens gerada pelo analisador léxico e transformá-la em uma Árvore Sintática Abstrata (AST).
  
- Analisador Semântico: responsável por pegar a AST do analisador sintático e devolver uma AST corrigida de acordo com as regras definidas nesta etapa. Gera mensagens de erro, conversões de tipos, advertências e é responsável pela validação de tudo o que veio da AST do analisador semântico.

<h1>Como Testar</h1>
Aqui já tem um arquivo teste.j--, se quiser testar com outros código recomendo colar o código neste arquivo teste pra não precisar alterar o caminho do arquivo no código Main.hs.

Com tudo na mesma pasta, você vai precisar ter o Alex, o Happy instalados e o GHC (Glasgow Haskell Compiler). Procure como faz para instalá-los com o seu gerenciador de pacotes.

Tendo tudo baixados, rode os seguintes comandos no terminal:
```bash
alex Lex.x
happy Parser.y
ghc Main.hs Semantico.hs Parser.hs Lex.hs AST.hs Token.hs -o compilador
./compilador
```
Com isso você será devidamente informado se funcionou ou não baseado nas mensagens de erro/sucesso que aparecem. Lembrando que: até agora só foi implementado até o analisador semântico, então só vai até a etapa de gerar a AST corrigida. Os bytecodes serão gerados na próxima etapa do trabalho.
