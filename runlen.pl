insert((_, 0), []) :- !.
insert((X, N), [X|Tail]) :- 
	!, % backtracking is not possible after this point
	N > 0,
	N1 is N - 1,
	insert((X, N1), Tail). % call the insert recursively for the tail of the list
insert(X, List) :- % in case we have a single element 
	insert((X,1), List).

decode_rl([], []). % base case
decode_rl([Element|Tail], List) :- % if the list is not empty
	insert(Element, List1),
	decode_rl(Tail, List2),
	append(List1, List2, List). % append the two lists
 
find_cons_pref([X,X|List], [X|Pref], Rest) :-
	find_cons_pref([X|List], Pref, Rest).
find_cons_pref([X,Y|List], [X], [Y|List]) :-
	X \= Y.
find_cons_pref([X], [X], []).

first([X|_], X).

add(X, 1, List1, NewList) :-
	append([X], List1, NewList).
add(X, N, List1, NewList) :-
	N > 1,
	append([(X,N)], List1, NewList).

encode_rl([], []). %base case
encode_rl(List, NewList) :-
	find_cons_pref(List, Pref, Rest),
	encode_rl(Rest, List1),
	first(Pref, X),
	length(Pref, N),
	add(X, N, List1, NewList).


/*  If we execute the following command : ?- encode_rl([p(3),p(X),q(X),q(Y),q(4)], L). in Eclipse the result we take is:

?- encode_rl([p(3), p(X), q(X), q(Y), q(4)], L).
X = 3
Y = 3
L = [(p(3), 2), (q(3), 2), q(4)]
Yes (0.00s cpu, solution 1, maybe more)
No (0.00s cpu)

As we can see, prolog tries to do pattern matching and to do so makes X equals to 3 and Y equals to 3 so now p(X) is now considered a term and not a variable.

*/