check([],[]) :-
	!.

check(X, X).

check(X, '*').

check([X|Tail1], [Y|Tail2]):-
	check(X,Y),
	check([Tail1], [Tail2]),
	!.	

star(*).

iddfs(List1, List2, List3, _, _, _, _):-
	check(List1, List2),
	writeln("List"),writeln(List1),
	!.

iddfs(List1, List2, List3, Solution, NewSolution, Depth, Depth) :-
	Max is Depth + 1,
	!,
	iddfs(List1, List2, List3, Solution, NewSolution, 0, Max).

iddfs(List1, List2, List3, Solution, NewSolution, CurrentDepth, MaxDepth) :-
	find_solution(List1, List2, List3, Solution, NewSolution, CurrentDepth),
	!.

iddfs(List1, List2, List3, Solution, NewSolution, CurrentDepth, MaxDepth) :-
	CurrentDepth =< MaxDepth,
	CurrentDepth1 is CurrentDepth + 1,
	iddfs(List1, List2, List3, Solution, NewSolution, CurrentDepth1, MaxDepth).

between(N1, N2, N1) :-
	N1 =< N2.

between(N1, N2, X) :-
	N1 < N2,
	NewN1 is N1 + 1,
	between(NewN1, N2, X).

legal_move(List1, List2, Solution, NewSolution) :-
	next_move(List1, List2, Solution, NewSolution).

legal_move(List1, List2, Solution, NewSolution) :-
	next_swap(List1, List2, Solution, NewSolution).

codegen(List1, List2, Solution):-
	iddfs(List1, List2, List3, [], Solution, 0, 1).

find_solution(List1, List2, _, Solution, _, _):-
	check(List1, List2),
	writeln(Solution),
	!.

find_solution(List1, List2, List3, Solution, NewSolution, CurrentDepth) :-
	CurrentDepth >= 0,
	Depth1 is CurrentDepth - 1,
	List1 \= List2,
	length(List1, N1),	
	length(List2, N2),
	N1 == N2,
	legal_move(List1, List3, Solution, NewSolution),
	find_solution(List3, List2, List4, NewSolution, NewSolution1, Depth1).

next_move(List1, List2, Solution, NewSolution) :-
	length(List1, N),
	between(1, N, X),
	append(Solution, [move(X)], NewSolution),
	move(X, List1, List2).

next_swap(List1, List2, Solution, NewSolution) :-
	length(List1, N),
	between(1, N, I),
	between(1, N, J),
	I =< J,
	append(Solution, [swap(I,J)], NewSolution),
	swap(I, J, List1, List2).

final_state([a, a, c]).

remove_first([],[]).
remove_first([_|Tail],Tail).

insert([],_,[]).

insert([Y|Tail],1,List1):-
	remove_first(Tail,Tail1) ,
	append([Y,Y], Tail1, List1).

insert([X|Tail], Pos, List):-
	Pos > 1,
	Pos1 is Pos - 1,
	insert(Tail, Pos1, List1),
	append([X], List1, List).

first([X|_], X).
last(X, [X]).
last(X, [_|Tail]) :- last(X, Tail).

insert_last(List, L):-
	remove_first(List, List1),
	last(X, List1),
	append([X], List1, L).

move(X, List, Result) :-
	X >= 1,
	length(List, N),
	(X < N -> insert(List, X, Result); insert_last(List, Result) ).

get_element(1, [X|_], X).
get_element(Pos, [_|Tail], Y):-
	Pos > 1,
	Pos1 is Pos - 1,
	get_element(Pos1, Tail, Y).

swap(X, Y, List, Result):-
	not(X = Y),
	X < Y,
	X > 0,
	Y > 0,
	length(List, N),
	X =< N,
	Y =< N,
	get_element(X, List, First),
	get_element(Y, List, Second),
	append(BeforeX, [First|AfterX], List),
	append(BeforeY, [Second|AfterY], List),
	intersection(BeforeY, AfterX, Between),
	append(BeforeX, [Second], R1),
	append(R1, Between, R2),
	append(R2, [First], R3),
	append(R3, AfterY, Result).


/* The only problem I have faced so far is that the final sequence of the steps is not returned correctly
but only the first element of it. Although the sequence id printed on the terminal so to check it is okay. */

