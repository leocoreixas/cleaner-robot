% Representação do mapa da sala
:- dynamic sala/1.

sala([
    [vazio, vazio, vazio, vazio, vazio],
    [vazio, sujeira, obstaculo, vazio, vazio],
    [vazio, vazio, vazio, vazio, vazio],
    [vazio, vazio, obstaculo, vazio, vazio],
    [vazio, vazio, vazio, vazio, sujeira]
]).

inicio(1,1).
fim(5,5).

% Definição das posições possíveis do robô
movimento(cima).
movimento(baixo).
movimento(esquerda).
movimento(direita).

% Verifica se uma posição é válida no mapa
posicao_valida(X, Y) :-
    sala(Mapa),
    nth1(Y, Mapa, Linha),
    nth1(X, Linha, _).

% Verifica se uma posição contém sujeira
posicao_sujeira(X, Y) :-
    sala(Mapa),
    nth1(Y, Mapa, Linha),
    nth1(X, Linha, sujeira).

% Verifica se uma posição contém obstáculo
posicao_obstaculo(X, Y) :-
    sala(Mapa),
    nth1(Y, Mapa, Linha),
    nth1(X, Linha, obstaculo).

% Verifica se uma posição está vazia (sem sujeira e sem obstáculo)
posicao_vazia(X, Y) :-
    posicao_valida(X, Y),
    \+ posicao_sujeira(X, Y),
    \+ posicao_obstaculo(X, Y).

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

% Heurística de custo (número de posições percorridas)
heuristica_custo(X1, Y1, X2, Y2, Custo) :-
    distancia_manhattan(X1, Y1, X2, Y2, Distancia),
    Custo is Distancia.

% Heurística de avaliação (distância de Manhattan)
heuristica_avaliacao(X1, Y1, X2, Y2, Avaliacao) :-
    distancia_manhattan(X1, Y1, X2, Y2, Avaliacao).

% Predicado para limpar uma posição (marcar como limpo)
limpar_posicao(X, Y) :-
    retract(sala(Mapa)),
    nth1(Y, Mapa, Linha),
    nth1(X, Linha, sujeira),
    replace(Linha, X, limpo, NovaLinha),
    replace(Mapa, Y, NovaLinha, NovoMapa),
    asserta(sala(NovoMapa)).

% Helper predicate to replace an element in a list
replace(List, Index, Element, Result) :-
    nth1(Index, List, _, Rest),
    nth1(Index, Result, Element, Rest).

% Predicado principal para percorrer a sala usando o algoritmo A*
best_first(X, Y, XFinal, YFinal, Caminho, Visitados) :-
    X = XFinal,
    Y = YFinal,
    Caminho = [].

best_first(X, Y, XFinal, YFinal, Caminho, Visitados) :-
    mover(X, Y, X2, Y2),
    \+ member(posicao(X2, Y2), Visitados),
    heuristica_avaliacao(X2, Y2, XFinal, YFinal, Avaliacao),
    best_first(X2, Y2, XFinal, YFinal, CaminhoRestante, [posicao(X2, Y2) | Visitados]),
    Caminho = [movimento(X, Y, X2, Y2) | CaminhoRestante].

% Algoritmo de busca Branch and Bound
branch_and_bound(X, Y, X, Y, [], _, Custo, Custo).
branch_and_bound(X, Y, XFinal, YFinal, Caminho, Visitados, CustoParcial, Custo) :-
    mover(X, Y, X2, Y2),
    \+ member(posicao(X2, Y2), Visitados),
    heuristica_custo(X2, Y2, XFinal, YFinal, CustoMovimento),
    NovoCustoParcial is CustoParcial + CustoMovimento,
    branch_and_bound(X2, Y2, XFinal, YFinal, CaminhoRestante, [posicao(X2, Y2) | Visitados], NovoCustoParcial, Custo),
    Caminho = [movimento(X, Y, X2, Y2) | CaminhoRestante].
branch_and_bound(_, _, _, _, [], _, _, _).


% Algoritmo de busca Hill Climbing
hill_climbing(X, Y, X, Y, [], _).
hill_climbing(X, Y, XFinal, YFinal, Caminho, Visitados) :-
    mover(X, Y, X2, Y2),
    \+ member(posicao(X2, Y2), Visitados),
    heuristica_avaliacao(X2, Y2, XFinal, YFinal, Avaliacao),
    heuristica_avaliacao(X, Y, XFinal, YFinal, HeuristicaAtual),
    (
        (Avaliacao < HeuristicaAtual, Caminho = [movimento(X, Y, X2, Y2) | CaminhoRestante], !);
        (Caminho = CaminhoRestante)
    ).
	
hill_climbing(_, _, _, _, [], _).


% Algoritmo de busca A*
a_star(X, Y, X, Y, [], _, Custo, Custo).
a_star(X, Y, XFinal, YFinal, Caminho, Visitados, CustoParcial, Custo) :-
    mover(X, Y, X2, Y2),
    \+ member(posicao(X2, Y2), Visitados),
    heuristica_custo(X2, Y2, XFinal, YFinal, CustoMovimento),
    NovoCustoParcial is CustoParcial + CustoMovimento,
    heuristica_avaliacao(X2, Y2, XFinal, YFinal, Avaliacao),
    NovoCusto is NovoCustoParcial + Avaliacao,
    a_star(X2, Y2, XFinal, YFinal, CaminhoRestante, [posicao(X2, Y2) | Visitados], NovoCustoParcial, Custo),
    Caminho = [movimento(X, Y, X2, Y2) | CaminhoRestante].
a_star(_, _, _, _, [], _, _, _).

% Predicado para limpar a sala usando o algoritmo A*
limpar_sala(Algoritmo) :-
    sala(Mapa),
    length(Mapa, Altura),
    length(Mapa, Largura),
    inicio(XInicial, YInicial),
    fim(XFinal, YFinal),
    (
        (Algoritmo = best_first, best_first(XInicial, YInicial, XFinal, YFinal, Caminho, [posicao(XInicial, YInicial)]));
        (Algoritmo = branch_and_bound, branch_and_bound(XInicial, YInicial, XFinal, YFinal, Caminho, [posicao(XInicial, YInicial)], 0, Altura * Largura));
		(Algoritmo = hill_climbing, hill_climbing(XInicial, YInicial, XFinal, YFinal, Caminho, [posicao(XInicial, YInicial)]));
        (Algoritmo = a_star, a_star(XInicial, YInicial, XFinal, YFinal, Caminho, [posicao(XInicial, YInicial)], 0, Altura * Largura))
    ),
    percorrer_caminho(Caminho).

% Predicado para percorrer o caminho e limpar as posições
percorrer_caminho([]).
percorrer_caminho([movimento(X1, Y1, X2, Y2) | Resto]) :-
    write('Movendo de '), write('('), write(X1), write(', '), write(Y1), write(')'),
    write(' para '), write('('), write(X2), write(', '), write(Y2), write(')'), nl,
    (
        (posicao_sujeira(X2, Y2), limpar_posicao(X2, Y2));
        (\+ posicao_sujeira(X2, Y2))
    ),
    percorrer_caminho(Resto).
    
    
 
