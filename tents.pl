:- lib(ic).
:- lib(branch_and_bound).
:- set_flag(print_depth, 1000).

tents(Row, Column, Trees, Tents):-
	length(Row, N),
	length(Column, M),
	N > 0,
	M > 0,
	Size is N * M,

	length(Positions, Size),
	Positions #:: 0..1,
	constraints(Positions, Trees, Row, Column, N, M),

	length(Positions1, Size),
	Positions1 #:: 0..1,
	constraints(Positions1, Trees, Row, Column, N, M),

	Cost #= sum(Positions),
	bb_min(search(Positions, 0, most_constrained, indomain, complete, []), Cost, bb_options{strategy:restart}),

	sum(Positions1) #= Cost,
	search(Positions1, 0, first_fail, indomain, complete, []),
	sol(Positions1, 1, 1, M, [], Tents).

%------------ Find Solution -------------%

sol([], _, _, _, R, R).

sol([X|T], RowCnt, ColumnCnt, M, R1, R):-
	X = 0,
	ColumnCnt >= 0,
	ColumnCnt < M,
	NColumnCnt is ColumnCnt + 1,
	sol(T, RowCnt, NColumnCnt, M, R1, R).

sol([X|T], RowCnt, ColumnCnt, ColumnCnt, R1, R):-
	X = 0,
	ColumnCnt >= 0,
	NRowCnt is RowCnt + 1,
	sol(T, NRowCnt, 1, ColumnCnt, R1, R).

sol([X|T], RowCnt, ColumnCnt, M, R1, R):-
	X = 1,
	ColumnCnt >= 0,
	ColumnCnt < M,
	NColumnCnt is ColumnCnt + 1,
	append(R1, [RowCnt - ColumnCnt], R2),
	sol(T, RowCnt, NColumnCnt, M, R2, R).

sol([X|T], RowCnt, ColumnCnt, ColumnCnt, R1, R):-
	X = 1,
	ColumnCnt >= 0,
	NRowCnt is RowCnt + 1,
	append(R1, [RowCnt - ColumnCnt], R2),
	sol(T, NRowCnt, 1, ColumnCnt, R2, R).

%----------- Positions --------------%

findX([X|_], 1, X):- !.
findX([_|T], J, R):-
	J > 1,
	NJ is J - 1,
	findX(T, NJ, R).

findPosition(TentsPositions, I, J, M, Tree):-
	I >= 0,
	J >= 0,
	M >= 0,
	Position is (I-1) * M + J,
	findX(TentsPositions, Position, Tree).

%------------ Constrains --------------%

constraints(Tents, Trees, Row, Column, N, M):-
	N > 0,
	M > 0,

	secondRule(Trees, Tents, N, M, [], TreesNeighbors),
	thirdRule(TreesNeighbors, Tents, N, M),

	split(Tents, M, TentsInRows),
	transpose(TentsInRows, TentsInColumns),

	firstRule(TentsInRows, Row),
	firstRule(TentsInColumns, Column).

split([], _, []).
split(L, M, [Row|Rows]):-
	M > 0,
	length(Row, M),
	append(Row, OtherRows, L),
	split(OtherRows, M, Rows).

transpose([[]|_], []).
transpose(L, [Col|T]):-
	findCol(L, Col, L1),
	transpose(L1, T).

findCol([], [], []).
findCol([[X|T1]|T2], [X|T3], [T1|T4]):-
	findCol(T2, T3, T4).

%-------------- Find Neighbors ----------------%

findNeighbors([], _, _, _, R, R).
findNeighbors([I-J|T], TentsPositions, N, M, R1, R):-
	findPosition(TentsPositions, I, J, M, X),
	append(R1, [X], R2),
	findNeighbors(T, TentsPositions, N, M, R2, R).

findNeighbors1(_, [], _, _, _).
findNeighbors1(X, [I-J|T], TentsPositions, N, M):-
	findPosition(TentsPositions, I, J, M, Y),
	X + Y #=< 1,
	findNeighbors1(X, T, TentsPositions, N, M).

neighbors(I, J, N, M, R1, R):-
	I >= 0,
	J >= 0,
	NI1 is I - 1,
	NJ1 is J - 1,
	NI2 is I + 1,
	NJ2 is J + 1,
	L = [NI1-NJ1, NI1-J, NI1-NJ2, I-NJ1, NI2-NJ1, NI2-J, NI2-NJ2],
	remove(L, N, M, [], R2),
	append(R1, R2, R).

%--------- Apply Rules/Constrains --------%

firstRule([], _).
firstRule([X|T], [Constrain|R]):-
	Constrain >= 0,
	sum(X) #=< Constrain,
	!,
	firstRule(T, R).
firstRule([_|T], [_|R]):-
	firstRule(T, R).

secondRule([], _, _, _, R, R).
secondRule([I-J|T], TentsPositions, N, M, R1, R):-
	N > 0,
	M > 0,
	findPosition(TentsPositions, I, J, M, Tree),
	Tree #= 0,
	I >= 0,
	J >= 0,
	neighbors(I, J, N, M, [], TreeNeighbors),
	append(R1, TreeNeighbors, R2),
	findNeighbors(TreeNeighbors, TentsPositions, N, M, [], Result),
	sum(Result) #>= 1,
	secondRule(T, TentsPositions, N, M, R2, R).

thirdRule([], _, _, _).
thirdRule([I-J|T], TentsPositions, N, M):-
	N > 0,
	M > 0,
	findPosition(TentsPositions, I, J, M, X),
	neighbors(I, J, N, M, [], Neighbors),
	findNeighbors1(X, Neighbors, TentsPositions, N, M),
	thirdRule(T, TentsPositions, N, M).

%--------- Remove everything Not in Matrix ----------%

remove([], _, _, R, R).

remove([I-J|T], N, M, R1, R):-
	I >= 1, 
	I =< N, 
	J >= 1, 
	J =< M, 
	!,
	append(R1, [I-J], R2),
	remove(T, N, M, R2, R).

remove([_|T], N, M, R1, R):-
	remove(T, N, M, R1, R).