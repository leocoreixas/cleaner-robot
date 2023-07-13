%Definição das Arestas do Grafo - Slide 38 - Busca Informada e
%Não-Informada - estado objetivo é o estado F 
%sG(G(V1,V2),V1,V2) - custo de mudar do estado V1 para o estado V2
sGB(63,a,b).
sGB(110,a,c).
sGB(53,a,e).
sGB(45,e,b).
sGB(65,b,d).
sGB(67,b,c).
sGB(45,c,d).
sGB(70,d,f).
sGB(52,e,f).
sGB(62,b,f).

%Grafo não-dirigido:
sG(G,V1,V2):-
    sGB(G,V1,V2).
sG(G,V1,V2):-
	sGB(G,V2,V1).

%sH(V,H(V,Obj)) - estimativa de custo para sair do estado ou nó V e 
%chegar ao estado objetivo Obj
sH(a,75).
sH(b,40).
sH(e,45).
sH(c,67).
sH(d,40).
sH(f,0).

%sF(G(n),H(n),F(n),VerticeOrigem,VerticeDestino) - usa a função F
sF(G,H,F,V1,V2):-sG(G,V1,V2),sH(V2,H),F is G + H.

% sH(G(n),H(n),F(n),VerticeOrigem,VerticeDestino) - usa somente a função
% de avaliação H
sH(H,V1,V2):-
    sGB(_,V1,V2),
    sH(V2,H).

% s(G(n),H(n),F(n),VerticeOrigem,VerticeDestino) - Não usa nenhuma
% heurística
s(V1,V2):-sG(_,V1,V2).

%Definir o nó (estado) objetivo
objetivo(f).

maior([_,_,F1|_],[_,_,F2|_]) :- F1 > F2.

/*relações acessórias para processamento de listas*/
membro(X,[X|_]):-!.
membro(X,[_|C]):-
    membro(X,C).

concatena([],L,L).
concatena([X|L1],L,[X|L2]):-
          concatena(L1,L,L2).



/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
ordenaF(Caminhos,CaminhosOrd)

Ordena os Caminhos a partir do primeiro valor de heurística dos caminhos em Caminhos
e retorna em CaminhosOrd.
+ <arg-1> Caminhos - lista de caminhos a ser ordenada
- <arg-2> Caminhos ordenados
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
ordenaF(Caminhos,CaminhosOrd):-
	quicksortF(Caminhos,CaminhosOrd).

particionarF(_,[],[],[]).
particionarF(X,[Y|Cauda],[Y|Menor],Maior):-
	maiorF(X,Y),!,
	particionarF(X,Cauda,Menor,Maior).
particionarF(X,[Y|Cauda],Menor,[Y|Maior]):-
	particionarF(X,Cauda,Menor,Maior).

quicksortF([],[]).
quicksortF([X|Cauda],ListaOrd):-
	particionarF(X,Cauda,Menor,Maior),
	quicksortF(Menor,MenorOrd),
	quicksortF(Maior,MaiorOrd),
	concatena(MenorOrd,[X|MaiorOrd],ListaOrd).

%maiorF retorna verdadeiro se o valor de heurística F1 da lista do caminho 
%é maior que o valor F2 da segunda lista
maiorF([F1|_],[F2|_]):-F1 > F2.

%------------------------------
%Relações estende para busca em largura ou utilizando 
%as funções de custo G, de avaliação H e F, somatório de G e H

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
estende(Caminho,NovosCaminhos).

Gera todos os Caminhos possiveis a partir de Caminho.
+ <arg-1> Caminho
- <arg-2> Novos caminhos possiveis a partir de caminho
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

estende([No|Caminho],NovosCaminhos):-
    findall([NovoNo,No|Caminho],
	       (
           	   s(No,NovoNo),
               not(membro(NovoNo,[No|Caminho]))
           ),
           NovosCaminhos).

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
estendeG(Caminho,NovosCaminhos).

Gera a partir de [G,NoAtual|Caminho] todos os Caminhos possiveis a partir de Caminho
utilizando somente a função de custo G. O G dos caminhos resultantes deve ser o somatório
do caminho atual G com o custo para os nós visitados
+ <arg-1> Caminho - [H,NoAtual|Caminho]
- <arg-2> Novos caminhos possiveis a partir de caminho 
(lista de todos os caminhos resultantes a partir de NoAtual)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/


/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
estendeH(Caminho,NovosCaminhos).

Gera a partir de [H,NoAtual|Caminho] todos os Caminhos possiveis a partir de Caminho
utilizando somente a função de avaliação H
+ <arg-1> Caminho - [H,NoAtual|Caminho]
- <arg-2> Novos caminhos possiveis a partir de caminho 
(lista de todos os caminhos resultantes a partir de NoAtual)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/
estendeH([_,No|Caminho],NovosCaminhos) :-
	findall([HNovo,NovoNo,No|Caminho],
	( 
		sH(HN,No,NovoNo),
		not(member(NovoNo,[No|Caminho])),
		HNovo is HN),
		NovosCaminhos
	).

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
estendeF(Caminho,NovosCaminhos).

Gera a partir de [F,G,H,NohAtual|Caminho] todos os Caminhos possiveis a partir de Caminho
utilizando função de custo G, função de avaliação H, e calculando 
F(NohNovo) = G(NohNovo) + H(NohNovo)
+ <arg-1> Caminho - [F,G,H,NoAtual|Caminho]
- <arg-2> Novos caminhos possiveis a partir de caminho
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

estendeF([_,GC,_,No|Caminho],NovosCaminhos):-
	findall([FNovo,GNovo,HNovo,NovoNo,No|Caminho],
	      (
          	  sF(GN,HN,_,No,NovoNo),
              not(member(NovoNo,[No|Caminho])),
              GNovo is GC + GN, 
          	  HNovo is HN, 
              FNovo is GNovo + HNovo
          ),
	      NovosCaminhos).

/*!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
aEstrela(PossiveisCaminhos,Solucao)

Dada uma lista de PossiveisCaminhos (inicialmente com somente um
possivel caminho percorrido, contendo somente o nó inicial), na qual o primeiro caminho
[NoAtual|Caminho] é o que deve ser analisado, este programa implementa o algoritmo 
de busca A* para encontrar o melhor caminho. A estratégia consiste em gerar 
uma lista de possíveis caminhos NovosCaminhos, concatenar com os Caminhos já existentes,
ordenar a lista completa segundo a função de avaliação F = G+H e seguir na busca.

+ <arg-1> Lista contendo os possiveis caminhos - Inicia com vazio
- <arg-2> Solucao para o problema
- <arg-3> Custo da solucao encontrada
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!*/

aEstrela([[G,_,_,No|Caminho]|_],Solucao,G):-	 %Gera a solucao se o noh sendo visitado eh um 							 %no objetivo
	objetivo(No),                                    %O noh gerado no passo anterior eh um noh 							 %objetivo
    reverse([No|Caminho],Solucao).

aEstrela([Caminho|Caminhos], Solucao, G) :-
	estendeF(Caminho, NovosCaminhos), %Gera novos caminhos
	concatena(Caminhos,NovosCaminhos,CaminhosTotal),
	ordenaF(CaminhosTotal,CaminhosTotOrd),
	aEstrela(CaminhosTotOrd, Solucao, G). 	%Coloca o noh corrente no caminho e continua a recursao

%Busca do no a até o nó f (objetivo(f))
%aEstrela([[0,0,0,a]],Solucao, G).