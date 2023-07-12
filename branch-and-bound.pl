% Definir o tamanho da sala
room_size(4, 4).

% Definir predicado para representar a posição da sujeira
goal_state((1, 1)).

% Definir predicado para representar a posição do obstáculo
obstacle(1, 2).

% Definir predicado para verificar se uma posição é válida (não colide com obstáculos)
valid_position((X, Y)) :-
    \+ obstacle(X, Y),
    room_size(MaxX, MaxY),
    X >= 0, X < MaxX,
    Y >= 0, Y < MaxY.

% Definir predicado para gerar os sucessores de um estado
successor((X, Y), (NextX, Y)) :- NextX is X + 1. % Movimento para a direita
successor((X, Y), (NextX, Y)) :- NextX is X - 1. % Movimento para a esquerda
successor((X, Y), (X, NextY)) :- NextY is Y + 1. % Movimento para cima
successor((X, Y), (X, NextY)) :- NextY is Y - 1. % Movimento para baixo

% Definir predicado para calcular a distância de Manhattan entre dois pontos
manhattan_distance((X1, Y1), (X2, Y2), Distance) :-
    Distance is abs(X1 - X2) + abs(Y1 - Y2).

% Definir predicado para calcular o custo do caminho atual
calculate_path_cost([_], 0).
calculate_path_cost([State1, State2 | Path], Cost) :-
    manhattan_distance(State1, State2, SegmentCost),
    calculate_path_cost([State2 | Path], RestCost),
    Cost is RestCost + SegmentCost.

% Definir predicado para buscar o caminho usando a busca Branch and Bound
branch_and_bound(CurrentState, Path) :-
    branch_and_bound([(CurrentState, [CurrentState], 0)], [], Path).

% Definir predicado para expandir um estado e obter seus sucessores
expand_state((State, Path, _), Successors) :-
    findall((NextState, [NextState | Path], NextCost), (
        successor(State, NextState),
        valid_position(NextState),
        \+ member(NextState, Path),
        manhattan_distance(State, NextState, Distance),
        calculate_path_cost([NextState | Path], PathCost),
        NextCost is PathCost + Distance
    ), Successors).

% Definir predicado para buscar o melhor caminho usando a busca Branch and Bound
branch_and_bound([], CurrentBestPath, CurrentBestPath).
branch_and_bound([(CurrentState, CurrentPath, CurrentCost) | Rest], CurrentBestPath, FinalBestPath) :-
    goal_state(CurrentState),
    calculate_path_cost(CurrentPath, PathCost),
    PathCost < CurrentCost,
    branch_and_bound(Rest, CurrentPath, FinalBestPath).
branch_and_bound([(CurrentState, CurrentPath, CurrentCost) | Rest], CurrentBestPath, FinalBestPath) :-
    expand_state((CurrentState, CurrentPath, CurrentCost), Expanded),
    append(Rest, Expanded, NewQueue),
    branch_and_bound(NewQueue, CurrentBestPath, FinalBestPath).

% Exemplo de uso:
%branch_and_bound((0, 0), Path).
