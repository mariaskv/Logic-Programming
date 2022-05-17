:- lib(ic).

constrain([], _, _):-
	!.
constrain([X|T], Sum, [L|R]):-
	L #= (Sum #< X),
	constrain(T, Sum, R).

liars(List, Liars):-
	length(List, N),
	length(Liars, N),
	Liars #:: 0..1,
	Sum #= sum(Liars), %sum of 1s or 0s in the result
	constrain(List, Sum, Liars),
	search(Liars, 0, input_order, indomain, complete, []).

