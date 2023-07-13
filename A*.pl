% Definir o tamanho da sala
room_size(4, 4).

% Definir predicado para representar a posição da sujeira
goal_state((1, 1)).

% Definir predicado para representar a posição do obstáculo
obstacle(1, 2).

% Definir predicado para verificar se uma posição é válida (não colide com obstáculos)
valid_position(((X, Y), _)) :-
    \+ obstacle(X, Y),
    room_size(MaxX, MaxY),
    X >= 0, X < MaxX,
    Y >= 0, Y < MaxY.

% Definir predicado para representar a posição inicial do robô
% initial_state((0, 0)).

% Definir predicado para gerar os sucessores de um estado
successor(((X, Y), Cost), ((NextX, Y), NewCost)) :- 
    NextX is X + 1,
    valid_position(((NextX, Y), _)),
    NewCost is Cost + 5. % Movimento para a direita

successor(((X, Y), Cost), ((NextX, Y), NewCost)) :- 
    NextX is X - 1,
    valid_position(((NextX, Y),_)),
    NewCost is Cost + 5. % Movimento para a esquerda

successor(((X, Y), Cost), ((X, NextY), NewCost)) :- 
    NextY is Y + 1,
    valid_position(((X, NextY),_)),
    NewCost is Cost + 5. % Movimento para cima

successor(((X, Y), Cost), ((X, NextY), NewCost)) :- 
    NextY is Y - 1,
    valid_position(((X, NextY), _)),
    NewCost is Cost + 5. % Movimento para baixo

% Definir predicado para calcular a distância de Manhattan entre dois pontos
manhattan_distance(((X1, Y1), _), (X2, Y2), Distance) :-
    Distance is abs(X1 - X2) + abs(Y1 - Y2).

accumulate_cost(StartState, [], StartState, 0).
accumulate_cost(StartState, [State | Path], CurrentState, TotalCost) :-
    successor(State, NextState),
    accumulate_cost(StartState, Path, CurrentState, PathCost),
    NextState = (_, NextCost), % Extrai o custo do próximo estado
    TotalCost is PathCost + NextCost.

% Definir predicado para avaliar os estados possíveis com base na heurística (distância de Manhattan + custo acumulado)
evaluate_states([State], _, State, _).
evaluate_states([State1, State2 | Rest], StartState, BestState, AccumulatedCost) :-
    goal_state(GoalState), % Obter a posição da sujeira. Funciona se tiver mais de uma sujeira?
    manhattan_distance(State1, GoalState, Distance1), % Veio até aqui!
    manhattan_distance(State2, GoalState, Distance2),
    accumulate_cost(StartState, [State1], _, Cost1),
    accumulate_cost(StartState, [State2], _, Cost2),
    F1 is Distance1 + Cost1,
    F2 is Distance2 + Cost2,
    (F1 =< F2 ->
        evaluate_states([State1 | Rest], StartState, BestState, AccumulatedCost)
    ;
        evaluate_states([State2 | Rest], StartState, BestState, AccumulatedCost)
    ).

% Predicado para verificar se uma posição não foi visitada
check_not_visited((X, Y), Path) :-
    var(Path).
    
check_not_visited((X, Y), Path) :-
    \+ member(((X, Y), _), Path).


% Definir predicado para escolher o próximo estado com base na heurística (A*)
choose_next_state(CurrentState, Path, NextState) :-
    bagof((Position, Cost), (
        successor(CurrentState, (Position, Cost)),
        valid_position((Position, _))
    ), Successors),
    evaluate_states(Successors, CurrentState, NextState, _),
    check_not_visited(NextState, Path),                
    append(Path, [NextState], NewPath),
    accumulate_cost(CurrentState, NewPath, _, _).


% Definir predicado para buscar o caminho usando a busca A* (A-star)
a_star(CurrentState, Path, TotalCost) :-
    goal_state(CurrentState),
    Path = [CurrentState], % Chegamos ao estado objetivo, retorna o caminho
    TotalCost = 0.
a_star(CurrentState, Path, TotalCost) :-
    choose_next_state(CurrentState, Path, NextState),
    a_star(NextState, [NextState | Path], TotalCost).




















% Definir o tamanho da sala
room_size(4, 4).

% Definir predicado para representar a posição da sujeira
:- dynamic(goal_state/1).
goal_state((1, 1)).
goal_state((2, 3)).
goal_state((3,3)).

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
calculate_path_cost([_, NextState | Path], Cost) :-
    calculate_path_cost([NextState | Path], RestCost),
    Cost is RestCost + 1.

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


branch_and_bound(CurrentState, Path) :-
    branch_and_bound([(CurrentState, [CurrentState], 0)], [], Path).

% Caso base: quando a lista de estados a serem explorados está vazia,
% o CurrentBestPath é o melhor caminho encontrado até o momento.
branch_and_bound([], CurrentBestPath, CurrentBestPath).

% Cláusula que é executada quando o estado atual é o estado objetivo.
% Verifica se o custo do caminho atual é menor que o custo atualmente armazenado.
% Se for, encontramos um caminho melhor e chamamos branch_and_bound/3 novamente
% com a lista vazia e o CurrentPath como FinalBestPath.
branch_and_bound([(CurrentState, CurrentPath, CurrentCost) | Rest], CurrentBestPath, FinalBestPath) :-
    goal_state(CurrentState),
    calculate_path_cost(CurrentPath, PathCost),
    PathCost < CurrentCost,
    branch_and_bound([], CurrentPath, FinalBestPath).

% Cláusula recursiva que expande o estado atual e obtém seus sucessores.
% Em seguida, anexa os sucessores à lista Rest e à Expanded.
% Chama branch_and_bound/3 novamente com a nova fila de estados (NewQueue).
branch_and_bound([(CurrentState, CurrentPath, CurrentCost) | Rest], CurrentBestPath, FinalBestPath) :-
    expand_state((CurrentState, CurrentPath, CurrentCost), Expanded),
    append(Rest, Expanded, NewQueue),
    branch_and_bound(NewQueue, CurrentBestPath, FinalBestPath).


start_branch_and_bound(StartState, Path) :-
    branch_and_bound(StartState, TempPath),
    reverse(TempPath, Path),!.

call_branch_and_bound([], []). 
call_branch_and_bound([H | List], [Path|FinalPath]) :-
	call_branch_and_bound(List, FinalPath),
    start_branch_and_bound(H, Path).

remove_last([_], []).
remove_last([X|Xs], [X|Rest]) :-
    remove_last(Xs, Rest).

branch_and_bound_all(FinalPath) :-
   	findall(Position, goal_state(Position), AllGoalPositions),
    append([(0,0)], AllGoalPositions, AllStartPositions),
    call_branch_and_bound(AllStartPositions, Path),
    remove_last(Path, FinalPath).



