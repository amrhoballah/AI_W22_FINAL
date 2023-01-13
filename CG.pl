:- include('KB.pl').

% Define the initial state of the agent
initState(S) :-
    agent_loc(X, Y),
    ships_loc(Ships),
    station(SX, SY),
    capacity(Capacity),
    S = [[X, Y], Ships, [SX, SY], Capacity, 0]. % State of the grid

% Define the goal predicate
goalCheck(S) :-
    S = [[_, _], [], [_, _], _,0]. % Check that the ship are all removed and guard ship is empty 
	
	
% Check the if am action is possible in the current state

isValid(pickup,S) :-
    [[X, Y], Ships, _, Capacity, OnGuard]=S,
    member([X, Y], Ships),
	OnGuard < Capacity.
	
isValid(drop,S) :-
    [[X, Y], _, [_, _], _,OnGuard]=S,
    station(X, Y),
	OnGuard > 0.
	
isValid(up,S) :-
    [[X, _], _, [_, _], _,_]=S,
    X2 is X - 1,
    X2 >= 0.

isValid(down,S) :-
    [[X, _], _, [_, _], _,_]=S,
    grid(N, _),
    X2 is X + 1,
    X2 < N.

isValid(left,S) :-
    [[_, Y], _, [_, _], _,_]=S,
    grid(_, _),
    Y2 is Y - 1,
    Y2 >= 0.
	
isValid(right,S) :-
    [[_, Y], _, [_, _], _,_]=S,
    grid(_, M),
    Y2 is Y + 1,
    Y2 < M.

% Define the action functions that change the state of the grid
action(S, pickup, S2) :-
    [[X, Y], Ships, Station, Capacity,OnGuard] = S, % get the information from state
    isValid(pickup,S), % check if pickup isValid is valid
    select([X, Y], Ships, Ships2), % remove [X, Y] from Ships
    OnGuard2 is OnGuard + 1, % add passengers to the coast guard ship
    S2 = [[X, Y], Ships2, Station, Capacity,OnGuard2]. % return the new state
	
action(S, drop, S2) :-
    isValid(drop,S),
    [[X, Y], Ships, Station, Capacity,_] = S,
    OnGuard2 is 0,
    S2 = [[X, Y], Ships, Station, Capacity,OnGuard2].
	
action(S, up, S2) :-
    isValid(up,S),
    [[X, Y], Ships, Station, Capacity,OnGuard] = S,
    X2 is X - 1,
	X2>=0,
    S2 = [[X2, Y], Ships, Station, Capacity,OnGuard].
	
action(S, down, S2) :-
    isValid(down,S),
    [[X, Y], Ships, Station, Capacity,OnGuard] = S,
    X2 is X + 1,
	grid(_,M),
	X2<M,
    S2 = [[X2, Y], Ships, Station, Capacity,OnGuard].
	
action(S, left, S2) :-
    isValid(left,S),
    [[X, Y], Ships, Station, Capacity,OnGuard] = S,
    Y2 is Y - 1,
	Y2>=0,
    S2 = [[X, Y2], Ships, Station, Capacity,OnGuard].
	
action(S, right, S2) :-
    isValid(right,S),
    [[X, Y],Ships, Station, Capacity,OnGuard]=S,
	Y2 is Y + 1,
	grid(N,_),
	Y2<N,
    S2 = [[X, Y2], Ships, Station, Capacity,OnGuard].


% Define the main goal function
goal(Path) :-
    initState(initState),
    search(initState, Path).


search(S, Path) :-
    dfs(S, initState,[S],Path).



% Define the depth-first search algorithm
dfs(S, Path,_,Path2) :-
    goalCheck(S), % check if we reached the goal state
	Path2 = Path,  % return the path and halt
	!.
dfs(S, Path,Visited,PathAfter) :-
    action(S, isValid, S2),  % call all the possible isValids
	\+ member(S2, Visited),  % check that the state has not been visited before
	addState(S2,Visited,New), % add the new visited state
	result(isValid, Path, Subpath), % add the path
    dfs(S2, Subpath,New,PathAfter). % recursive call
	
	
	
addState(S, Visited,New) :-
    append(Visited, [S], Visited2), % append S to the end of Visited
    New=Visited2. % update Visited with the new list Visited2



result(isValid, Subpath,Path) :- % adds to the path
    Path = result(isValid, Subpath).