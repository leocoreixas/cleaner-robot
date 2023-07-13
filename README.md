# Busca Heurística - Robô Explorador


## Descrição do Problema

Implemente em Prolog um programa que simule um robô para limpeza de uma sala. 

Considere as seguintes características para construir a solução para o problema

- Mapeie a sala como um tabuleiro. Nesse sentido, a sala deve ser representada como uma matriz, análoga a um tabuleiro (toda a sala é previamente mapeada).
     . Sugestão de estrutura: A sala pode ser representada por uma estrutura como vetor de vetores em Prolog manipulada internamente ou pode ser uma relação que verifica se uma dada posição, representada por uma relação posicao(...), possui sujeira ou obstáculo. Para facilitar, vocês podem inicialmente desenhar o mapa e depois trabalhar nas representações a partir do desenho.
- A sala possui obstáculos e as sujeiras estão armazenados em posições na sala. Assim, cada posição da sala pode possuir estruturas para armazenar sujeira ou obstáculo em uma posição da sala, caso existam. 
- Considere que o robô muda de estado ao se mover para uma posição à esquerda, à direita ou outras posições que vocês achem que é possível (diagonal, para frente ou para trás).
- Construa relações necessárias para representar a mudança de estado do robô, quando ele troca de posição na sala. 
- Construa uma heurística de custo e uma de avaliação para encontrar o caminho do robô. 
- O caminho a ser percorrido pelo robô é percorrer todos os estados com sujeira para limpar toda a sala, de tal maneira que percorra o menor percurso possível. 
- Estabeleça uma posição inicial e uma posição final no mapa da sala para serem os estados iniciais e finais. 
- Ao visitar um estado (posição) com sujeira, marque-o como limpo. Para isso, podem ser utilizados predicados que gerenciam a base de fatos em Prolog, dentre eles assert/1, que insere um fato na base de fatos, e retract/1, que remove um fato na base de fatos. Mais informações em https://www.swi-prolog.org/pldoc/man?section=dynpreds.

Avalie a performance dos algoritmos Best First, Branch and Bound, Hill Climbing e A* para encontrar o percurso de menor custo. Para calcular o custo no mapa, considere que cada posição do tabuleiro é de 1x1 unidade. Assim, o custo é o número de posições percorridas e a avaliação deve ser calculada como a distância de Manhattan. Para o cálculo da distância de Manhattan, veja em https://edisciplinas.usp.br/pluginfile.php/5309062/mod_resource/content/1/AM_Aula06.pdf#:~:text=A%20dist%C3%A2ncia%20de%20Manhattan%20(%E2%80%9CCity,diferen%C3%A7as%20absolutas%20de%20suas%20coordenadas. - Slide 6, pg 3.

Além do código em Prolog, faça também um relatório no docs do google drive que explique como vocês implementaram o problema, quais os resultados obtidos pelos 4 algoritmos de busca e uma análise do desempenho das estratégias selecionadas. Vocês deverão apresentar presencialmente em slots a serem reservados para mim, na minha sala. O meu objetivo na apresentação é perceber se foram vocês mesmos que implementaram, quais as dificuldades que vocês enfrentaram, como vocês resolveram as dificuldades e quais as limitações que vocês percebem da implementação de vocês. Para atribuir uma nota, vou considerar que o grupo que apresentar a melhor solução, desde que tenha todas as especificações apresentadas e resolva o problema, é nota 10, e vou reduzir a nota dos demais conforme as limitações apresentadas.


## Pré-requisitos

 Para executar o projeto é necessário ter instalado:

- [SWI-Prolog](https://www.swi-prolog.org/Download.html)

Ou então utilizar o [SWISH](https://swish.swi-prolog.org/) - Versão online


## Soluções implementadas

#### Busca best-first

Para executar a busca best-first, basta fazer a seguinte pergunta ao SWI-Prolog:

```best_first_all((0,0), FinalPath).``` 

Onde (0,0) é a posição inicial do robô e FinalPath é a variável que irá armazenar o caminho percorrido pelo robô.
<hr/>

### Busca branch-and-bound
Para executar a busca branch-and-bound, basta fazer a seguinte pergunta ao SWI-Prolog:

```branch_and_bound_all(FinalPath).``` 

Onde FinalPath é a variável que irá armazenar o caminho percorrido pelo robô.
<hr/>

### Busca hill-climbing
Para executar a busca hill-climbing, basta fazer a seguinte pergunta ao SWI-Prolog:

```hill_climbing_all((0,0), FinalPath).``` 

Onde (0,0) é a posição inicial do robô e FinalPath é a variável que irá armazenar o caminho percorrido pelo robô.
<hr/>

### Busca A*
Para executar a busca A*, basta fazer a seguinte pergunta ao SWI-Prolog:

```a_star_all(FinalPath).``` 

Onde FinalPath é a variável que irá armazenar o caminho percorrido pelo robô.

<hr/>


# Relatório de Desenvolvimento

- [Busca Heurística - Robô para limpeza de uma sala](https://docs.google.com/document/d/1bYtIsm2Q0PgKeF8gdb92Rx9RD5NCqrh1_jKi4y9QOjo/edit?usp=sharing)


# Slides de apresentação

- [Busca Heurística - Slides de apresentação](https://docs.google.com/presentation/d/157yu-gMNox5UoAAREjEtTLP7WVUqBpeE1N2m_Nu7cwY/edit?usp=sharing)


# Alunos

 - [João Victor Simonassi - 217031149](https://github.com/jsimonassi)
 - [Leonardo Coreixas - 217031137](https://github.com/leocoreixas)