% Representação do mapa da sala
:- dynamic sala/1.

sala([
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _],
    [_, _, _, _, _]
]).

% Definição das posições iniciais e final
inicio(0, 0).
fim(4, 4).

% Definição das posições com sujeira
sujeira(3, 2).
sujeira(4, 4).

% Definição das posições com obstáculos
obstaculo(2, 3).
obstaculo(3, 3).

% Definição das posições possíveis do robô
movimento(cima).
movimento(baixo).
movimento(esquerda).
movimento(direita).

% Verifica se uma posição é válida no mapa
posicao_valida(X, Y) :-
    sala(Mapa),
    nth0(Y, Mapa, Linha),
    nth0(X, Linha, _),
    \+ obstaculo(X, Y),
    var(Elemento). %  verifica se a variável Elemento é uma variável não instanciada


% Movimenta o robô para uma nova posição
mover(X, Y, X2, Y2) :-
    movimento(Movimento),
    (
        (Movimento = cima, X2 is X, Y2 is Y - 1);
        (Movimento = baixo, X2 is X, Y2 is Y + 1);
        (Movimento = esquerda, X2 is X - 1, Y2 is Y);
        (Movimento = direita, X2 is X + 1, Y2 is Y)
    ),
    posicao_valida(X2, Y2).


% Calcula a distância de Manhattan entre duas posições
distancia_manhattan(X1, Y1, X2, Y2, Distancia) :-
    Distancia is abs(X2 - X1) + abs(Y2 - Y1).

% Heurística de avaliação (distância de Manhattan)
heuristica_avaliacao(X1, Y1, X2, Y2, Avaliacao) :-
    distancia_manhattan(X1, Y1, X2, Y2, Avaliacao).





% Verifica se a posição atual é o objetivo
chegou_ao_objetivo(X, Y, ObjetivoX, ObjetivoY) :-
    X = ObjetivoX,
    Y = ObjetivoY.

% Verifica se a posição atual é uma posição já visitada
ja_visitado(_, _, []).
ja_visitado(X, Y, [posicao(X2, Y2) | Resto]) :-
    X = X2,
    Y = Y2;
    ja_visitado(X, Y, Resto).

% Encontra o melhor movimento a partir da posição atual
melhor_movimento(X, Y, ObjetivoX, ObjetivoY, Movimento) :-
    findall(
        Distancia,
        (mover(X, Y, X2, Y2), \+ ja_visitado(X2, Y2, Movimento), distancia_manhattan(X2, Y2, ObjetivoX, ObjetivoY, Distancia)),
        Movimentos
    ),
    sort(Movimentos, MovimentosOrdenados),
    MovimentosOrdenados = [_ | _], % Verifica se a lista não está vazia
    min_member(MinDistancia, MovimentosOrdenados),
    mover(X, Y, X3, Y3),
    distancia_manhattan(X3, Y3, ObjetivoX, ObjetivoY, MinDistancia).

% Algoritmo de Hill Climbing
hill_climbing(X, Y, ObjetivoX, ObjetivoY, Caminho, Visitados) :-
    chegou_ao_objetivo(X, Y, ObjetivoX, ObjetivoY),
    reverse([posicao(X, Y) | Visitados], Caminho).

hill_climbing(X, Y, ObjetivoX, ObjetivoY, Caminho, Visitados) :-
    melhor_movimento(X, Y, ObjetivoX, ObjetivoY, Movimento),
    mover(X, Y, X2, Y2),
    \+ ja_visitado(X2, Visitados, Y2),
    hill_climbing(X2, Y2, ObjetivoX, ObjetivoY, Caminho, [posicao(X, Y) | Visitados]).





% Predicado para limpar a sala usando o algoritmo A*
%limpar_sala(Algoritmo) :-
%    sala(Mapa),
%    length(Mapa, Altura),
%    nth0(0, Mapa, PrimeiroElemento),
%    length(PrimeiroElemento, Largura),
%    inicio(XInicial, YInicial),
%    sujeira(Objetivo1X, Objetivo1y), %Trocar para iteração
%    hill_climbing(XInicial, YInicial, Objetivo1X, Objetivo1y, Caminho, []).
    %percorrer_caminho(Caminho).

    

