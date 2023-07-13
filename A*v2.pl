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

%Cláusula inicial. Chama a_star/3 com a lista de estados a serem explorados
% Possui apenas DOIS parâmetros. Isso é importante!
a_star(CurrentState, Path) :-
    a_star([(CurrentState, [CurrentState], 0)], [], Path).

% Caso base: quando a lista de estados a serem explorados está vazia,
% o CurrentBestPath é o melhor caminho encontrado até o momento.
a_star([], CurrentBestPath, CurrentBestPath).

% Cláusula que é executada quando o estado atual é o estado objetivo.
% Verifica se o custo do caminho atual é menor que o custo atualmente armazenado.
% Se for, encontramos um caminho melhor e chamamos a_star/3 novamente
% com a lista vazia e o CurrentPath como FinalBestPath.
a_star([(CurrentState, CurrentPath, CurrentCost) | Rest], CurrentBestPath, FinalBestPath) :-
    goal_state(CurrentState),
    calculate_path_cost(CurrentPath, PathCost),
    PathCost < CurrentCost,
    a_star([], CurrentPath, FinalBestPath).

% Cláusula recursiva que expande o estado atual e obtém seus sucessores.
% Em seguida, anexa os sucessores à lista Rest e à Expanded.
% Chama a_star/3 novamente com a nova fila de estados (NewQueue).
a_star([(CurrentState, CurrentPath, CurrentCost) | Rest], CurrentBestPath, FinalBestPath) :-
    expand_state((CurrentState, CurrentPath, CurrentCost), Expanded),
    append(Rest, Expanded, NewQueue),
    a_star(NewQueue, CurrentBestPath, FinalBestPath).


start_a_star(StartState, Path) :-
    a_star(StartState, TempPath),
    reverse(TempPath, Path),!.

call_a_star([], []). 
call_a_star([H | List], [Path|FinalPath]) :-
	call_a_star(List, FinalPath),
    start_a_star(H, Path).

remove_last([_], []).
remove_last([X|Xs], [X|Rest]) :-
    remove_last(Xs, Rest).

a_star_all(FinalPath) :-
   	findall(Position, goal_state(Position), AllGoalPositions),
    append([(0,0)], AllGoalPositions, AllStartPositions),
    call_a_star(AllStartPositions, Path),
    remove_last(Path, FinalPath).


%start(FinalPath) :-
%    statistics(runtime, _),
%    a_star_all(FinalPath),
%    statistics(runtime, [_, ExecutionTime]),
%    format('Tempo de execução: ~15f segundos', [ExecutionTime/1000]).

