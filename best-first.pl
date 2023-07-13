%Definir o tamanho da sala
room_size(4,4).

% Definir predicado para representar a posição da sujeira
:- dynamic(goal_state/1).
goal_state((1, 1)).
goal_state((2, 3)).
goal_state((3, 3)).

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
evaluate_states(NextStates, NextState) :-
    goal_state(GoalState),
    map_list_to_pairs(manhattan_distance(GoalState), NextStates, HeuristicPairs),
    keysort(HeuristicPairs, SortedPairs),
    pairs_values(SortedPairs, SortedStates),
    [NextState | _] = SortedStates.

% Definir predicado para buscar o caminho usando a busca Best-First
best_first(CurrentState, Path) :-
    goal_state(CurrentState),
    Path = [CurrentState]. % Chegamos ao estado objetivo, retorna o caminho
best_first(CurrentState, Path) :-
    choose_next_state(CurrentState, NextState),
    best_first(NextState, NextPath),
    Path = [CurrentState | NextPath]. % Constrói o caminho

get_last([X], X).
get_last([_|Xs], Rest) :-
    get_last(Xs, Rest).

best_first_all(CurrentState, FinalPath) :-
    best_first(CurrentState, Path),
    get_last(Path, Result),
    retract(goal_state(Result)),
    append(Path, FinalPath, FinalPath).

%Exemplo de chamada
%best_first_all((0,0), FinalPath).
