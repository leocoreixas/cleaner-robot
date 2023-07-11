% Definir o tamanho da sala
room_size(4,4).

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
calculate_path_cost([State | Path], Cost) :-
    calculate_path_cost([State | Path], 0, Cost).

calculate_path_cost([_], Cost, Cost).
calculate_path_cost([State1, State2 | Path], AccCost, Cost) :-
    manhattan_distance(State1, State2, SegmentCost),
    NewAccCost is AccCost + SegmentCost,
    calculate_path_cost([State2 | Path], NewAccCost, Cost).

% Definir predicado para buscar o caminho usando a busca Branch and Bound
branch_and_bound(CurrentState, Path) :-
    calculate_path_cost([CurrentState], CurrentCost),
    branch_and_bound([(CurrentState, CurrentCost)], [], Path).

% Definir predicado auxiliar para buscar o caminho usando a busca Branch and Bound
branch_and_bound([], CurrentBestPath, CurrentBestPath).
branch_and_bound([(CurrentState, CurrentCost) | Rest], CurrentBestPath, FinalBestPath) :-
    goal_state(CurrentState),
    calculate_path_cost([CurrentState | Rest], CurrentPathCost),
    CurrentPathCost < CurrentCost,
    branch_and_bound(Rest, [CurrentState | Rest], FinalBestPath).
branch_and_bound([(CurrentState, CurrentCost) | Rest], CurrentBestPath, FinalBestPath) :-
    expand_state((CurrentState, CurrentCost), Expanded),
    append(Rest, Expanded, NewQueue),
    select_best_state(NewQueue, Selected),
    branch_and_bound(Selected, CurrentBestPath, FinalBestPath).

% Definir predicado para expandir um estado e obter seus sucessores
expand_state((State, _), Successors) :-
    findall((NextState, NextCost), (
        successor(State, NextState),
        valid_position(NextState),
        manhattan_distance(NextState, State, Distance),
        NextCost is Distance + 1
    ), Successors).

% Definir predicado para selecionar o melhor estado com menor custo
select_best_state([State | Rest], Selected) :-
    select_best_state(Rest, State, Selected).

select_best_state([], BestState, BestState).
select_best_state([(State, Cost) | Rest], (BestState, BestCost), Selected) :-
    Cost < BestCost,
    select_best_state(Rest, (State, Cost), Selected).
select_best_state([_ | Rest], (BestState, BestCost), Selected) :-
    select_best_state(Rest, (BestState, BestCost), Selected).

% Exemplo de uso:
%branch_and_bound((0, 0), Path).
