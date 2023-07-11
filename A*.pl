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

% Definir predicado para escolher o próximo estado com base na heurística
choose_next_state(CurrentState, NextState) :-
    bagof(NextState, (successor(CurrentState, NextState), valid_position(NextState)), NextStates),
    evaluate_states(NextStates, CurrentState, NextState).

% Definir predicado para calcular a distância de Manhattan entre dois pontos
manhattan_distance((X1, Y1), (X2, Y2), Distance) :-
    Distance is abs(X1 - X2) + abs(Y1 - Y2).

% Definir predicado para avaliar os estados possíveis com base na heurística (distância de Manhattan)
evaluate_states([State], _, State).
evaluate_states([State1, State2 | Rest], CurrentState, BestState) :-
    goal_state(GoalState), % Obter a posição da sujeira. Funciona se tiver mais de uma sujeira?
    manhattan_distance(State1, GoalState, Distance1),
    manhattan_distance(State2, GoalState, Distance2),
    manhattan_distance(CurrentState, GoalState, CurrentDistance),
    (Distance1 + CurrentDistance =< Distance2 + CurrentDistance ->
        evaluate_states([State1 | Rest], CurrentState, BestState)
    ;
        evaluate_states([State2 | Rest], CurrentState, BestState)
    ).

% Definir predicado para buscar o caminho usando a busca Hill Climbing
a_start(CurrentState, Path) :-
    goal_state(CurrentState),
    Path = [CurrentState]. % Chegamos ao estado objetivo, retorna o caminho
a_start(CurrentState, Path) :-
    choose_next_state(CurrentState, NextState),
    hill_climbing(NextState, NextPath),
    Path = [CurrentState | NextPath]. % Constrói o caminho

%a_start((0, 0), Path).