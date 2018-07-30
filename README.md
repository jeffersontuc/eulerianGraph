# Eulerian Graph

## Tell if a graph is eulerian or not


*Web project made for Functional Programming course at UFCG using scotty framework.*


## Comandos importantes

- stack build (Comando inicial. Instala as bibliotecas que foram importadas no eulerianGraph.cabal)
- stack ghci (Abre o terminal ghci onde você pode executar as funções definidas no arquivo app/Main.hs)

- Foi adicionado um arquivo gs.hs na pasta app com o propósito de executar o código através do terminal (Sem servidor)

# Rodar o servidor (Precisa ter o stack instalado)

- Comentar/Remover o bloco main que existe no arquivo app/gs.hs (O stack não compilará se houver conflito de mains)
- stack build
- stack ghci
- >> main (no terminal que irá abrir ao executar o comando anterior)


## Rodar o client (Precisa de Nodejs instalado na máquina)

- cd client
- npm install
- node index.js


## Links úteis

* [Instalar o stack] https://docs.haskellstack.org/en/stable/README/

* [Web server básico com scotty] http://seanhess.github.io/2015/08/19/practical-haskell-json-api.html
