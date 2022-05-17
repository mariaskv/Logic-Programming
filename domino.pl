dominos([(0,0),(0,1),(0,2),(0,3),(0,4),(0,5),(0,6),
 (1,1),(1,2),(1,3),(1,4),(1,5),(1,6),
 (2,2),(2,3),(2,4),(2,5),(2,6),
 (3,3),(3,4),(3,5),(3,6),
 (4,4),(4,5),(4,6),
 (5,5),(5,6),
 (6,6)]).

frame([	[3,1,2,6,6,1,2,2],
 	[3,4,1,5,3,0,3,6],
 	[5,6,6,1,2,4,5,0],
 	[5,6,4,1,3,3,0,0],
 	[6,1,0,6,3,2,4,0],
 	[4,1,5,2,4,3,5,5],
 	[4,1,0,2,4,5,2,0]]).

/*_________________Find Pairs_________________*/

/* Cross the frame and keep all the combinations of two numbers in a row
or in a column in order to track them easier */

pairs_in_row(_, [_], S, S) :-
	!.
pairs_in_row(Pos, [X,Y|T], Pairs, S):-
	Pos > 0,
	NextPos is Pos + 1,
	append([(X,Y,Pos,NextPos)], Pairs, NewPairs),
	pairs_in_row(NextPos, [Y|T], NewPairs, S).

pairs_in_column([], [], _, _, _, _).

pairs_in_column([X], [Y], Pos, Size, Pairs, S):-
	Pos > 0,
	Size > 0,
	DownPos is Pos + Size,
	append([(X,Y,Pos,DownPos)], Pairs, S),
	!.

pairs_in_column([X|T1], [Y|T2], Size, Pos, Pairs, S):-
	Pos > 0,
	Size > 0,
	DownPos is Pos + Size,
	append([(X,Y,Pos,DownPos)], Pairs, NewPairs),
	NextPos is Pos + 1,
	pairs_in_column(T1, T2, Size, NextPos, NewPairs, S).	

all_pairs([X], Pos, _, P1, P):-
	Pos > 0,
	pairs_in_row(Pos, X, [], P2),
	append(P1, P2, P),
	!.

all_pairs([X,Y|T], Pos, Size, P1, P):-
	Pos > 0,
	Size > 0,
	pairs_in_row(Pos, X, [], P2),
	pairs_in_column(X, Y, Size, Pos, [], P3),
	append(P1, P2, P4),
	append(P4, P3, P5),
	NextPos is Pos + Size,
	all_pairs([Y|T], NextPos, Size, P5, P).

/*______________________Find All Combinations_________________*/
/*Find where our given dominos can be putted on the frame based
on the pairs list we made previously*/

all_positions(_, [_], R, R):- !.
all_positions(P, [(X,Y)|T], A, R):-
	position((X,Y), P, [], S),
	append(A, [S], A1),
	all_positions(P, T, A1, R).	

position(_, [_], R, R).
position((X,Y), [(X,Y,C,D)|T], S, R):-
	append(S, [(X,Y,C,D)], S1),
	!,
	position((X,Y),T, S1,R).

position((X,Y), [(Y,X,C,D)|T], S, R):-
	append(S, [(X,Y,D,C)], S1),
	!,
	position((X,Y),T, S1,R).

position((X,Y), [(_,_,_,_)|T],S,R):-
	position((X,Y),T,S,R).


/* starting from the uper left corner witch is counted to 0 we move on and cross the frame 
as a snake and every domino has a unique number */

head_length([X|_],L) :- length(X,L).

/*________________Solve_________________*/
/*cross the domain and pick up a solution for every domino
and remove this position from all the other domains*/

remove_if_exists(_, [], []).
remove_if_exists(X, [X|List], List) :-
   !.
remove_if_exists(X, [Y|List1], [Y|List2]) :-
   remove_if_exists(X, List1, List2).

solve([], S, S):-
	!.
solve([[(A,B,X,Y)|_]|T], S, Sol):-
	remove_if_exists([(A,B,X,Y)],T,R),
	append(S, [[(A,B,X,Y)]], S1),
	solve(R, S1, Sol).

/*___________________Print Solution_________________*/
printf([]).
printf([(X,Y,_,_)]):-
	write(X),
	write("-"),
	write(Y),
	nl,
	!.	
printf([[(X,Y,A,B)]|Tail2]):-
	write(X),
	write("-"),
	write(Y),	
	write("-"),
	write(A),
	write("-"),
	write(B),
	nl,
	printf(Tail2).

put_dominos:-
	frame(F),
	head_length(F, L1),
	all_pairs(F, 1, L1, [], P),
	dominos(D),
	all_positions(P, D, [], Pos),
	solve(Pos, [], Sol),
	printf(Sol).















