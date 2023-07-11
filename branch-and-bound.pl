%Definir o tamanho da sala
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

% Definir predicado para representar a posição inicial do robô
%initial_state((0, 0)).

% Definir predicado para gerar os sucessores de um estado
successor((X, Y), (NextX, Y)) :- NextX is X + 1. % Movimento para a direita
successor((X, Y), (NextX, Y)) :- NextX is X - 1. % Movimento para a esquerda
successor((X, Y), (X, NextY)) :- NextY is Y + 1. % Movimento para cima
successor((X, Y), (X, NextY)) :- NextY is Y - 1. % Movimento para baixo

% Definir predicado para calcular a distância de Manhattan entre dois pontos
manhattan_distance((X1, Y1), (X2, Y2), Distance) :-
    Distance is abs(X1 - X2) + abs(Y1 - Y2).


% Definir predicado para escolher o próximo estado com base na heurística
choose_next_state(CurrentState, NextState) :-
    bagof(NextState, (successor(CurrentState, NextState), valid_position(NextState)), NextStates),
    evaluate_states(NextStates, NextState).

% Definir predicado para avaliar os estados possíveis com base na heurística (distância de Manhattan)
evaluate_states([State], State).
evaluate_states([State1, State2 | Rest], BestState) :-
    goal_state(GoalState),
    manhattan_distance(State1, GoalState, Distance1),
    manhattan_distance(State2, GoalState, Distance2),
    (Distance1 =< Distance2 ->
        evaluate_states([State1 | Rest], BestState)
    ;
        evaluate_states([State2 | Rest], BestState)
    ).

% Definir predicado para buscar o caminho usando a busca Branch and Bound
branch_and_bound(CurrentState, Path) :-
    goal_state(CurrentState),
    Path = [CurrentState]. % Chegamos ao estado objetivo, retorna o caminho
branch_and_bound(CurrentState, Path) :-
    choose_next_state(CurrentState, NextState),
    branch_and_bound(NextState, NextPath),
    Path = [CurrentState | NextPath]. % Constrói o caminho

% Definir predicado para calcular a distância total percorrida em um caminho
calculate_path_distance([], 0).
calculate_path_distance([State1, State2 | Rest], Distance) :-
    manhattan_distance(State1, State2, SegmentDistance),
    calculate_path_distance([State2 | Rest], RestDistance),
    Distance is SegmentDistance + RestDistance.

% Exemplo de uso:
%branch_and_bound((0, 0), Path).
