% Representação do mapa da sala
:- dynamic sala/1.

sala([
    [_, _, _, _, _],
    [_, s, _, _, _],
    [_, _, _, _, _],
    [_, _, o, o, _],
    [_, _, _, _, _]
]).

sujeira(1,1).

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
    \+ obstaculo(X, Y).

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
    

% Verifica se a posição atual é o objetivo
chegou_ao_objetivo(X, Y, ObjetivoX, ObjetivoY) :-
    X = ObjetivoX,
    Y = ObjetivoY.

% Verifica se a posição atual é uma posição já visitada
ja_visitado(X, Y, [posicao(X1, Y1) | _]) :-
    X = X1,
    Y = Y1.
ja_visitado(X, Y, [_ | Resto]) :-
    ja_visitado(X, Y, Resto).

% Encontra o melhor movimento a partir da posição atual
melhor_movimento(X, Y, ObjetivoX, ObjetivoY, Movimento, MelhorDistancia) :-
    movimento(Movimento),
    mover(X, Y, X2, Y2),
    \+ ja_visitado(X2, Y2, Movimento),
    distancia_manhattan(X2, Y2, ObjetivoX, ObjetivoY, Distancia),
    (
        MelhorDistancia = -1, % Inicializa a melhor distância se for a primeira iteração
        melhor_movimento(X, Y, ObjetivoX, ObjetivoY, Movimento, Distancia)
    ;
        Distancia < MelhorDistancia, % Atualiza a melhor distância se encontrarmos uma menor
        melhor_movimento(X, Y, ObjetivoX, ObjetivoY, Movimento, Distancia)
    ;
        true % Não faz nada se a distância não for melhor
    ).

% Algoritmo de Hill Climbing
hill_climbing(X, Y, ObjetivoX, ObjetivoY, Caminho, Visitados) :-
    chegou_ao_objetivo(X, Y, ObjetivoX, ObjetivoY),
    reverse([posicao(X, Y) | Visitados], Caminho).

hill_climbing(X, Y, ObjetivoX, ObjetivoY, Caminho, Visitados) :-
    melhor_movimento(X, Y, ObjetivoX, ObjetivoY, _, -1), % Inicializa a melhor distância como -1
    mover(X, Y, X2, Y2),
    \+ ja_visitado(X2, Y2, Visitados),
    hill_climbing(X2, Y2, ObjetivoX, ObjetivoY, Caminho, [posicao(X, Y) | Visitados]).



limpar_sala(Caminho):-
    sujeira(X,Y),
    hill_climbing(0, 0, X, Y, Caminho, []).
