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
        calculate_path_cost([NextState | Path], PathCost),
        NextCost is PathCost + 1
    ), Successors).

%Cláusula inicial. Chama branch_and_bound/3 com a lista de estados a serem explorados
% Possui apenas DOIS parâmetros. Isso é importante!
branch_and_bound(CurrentState, Path) :-
    branch_and_bound([(CurrentState, [CurrentState], 0)], [], Path).

% Caso base: quando a lista de estados a serem explorados está vazia,
% Nesse caso, o CurrentBestPath é o melhor caminho encontrado até o momento,
% então o predicado termina e retorna o CurrentBestPath como FinalBestPath.
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


%start(FinalPath) :-
%    statistics(runtime, _),
%    branch_and_bound_all(FinalPath)
%    statistics(runtime, [_, ExecutionTime]),
%    format('Tempo de execução: ~15f segundos', [ExecutionTime/1000]).
