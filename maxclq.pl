:-lib(ic).
:-lib(branch_and_bound).
 
%%%%%%%%%%%% Given code for graph create %%%%%%%%%%%%%%%%%%%%%%%

create_graph(NNodes, Density, Graph) :-
   cr_gr(1, 2, NNodes, Density, [], Graph).

cr_gr(NNodes, _, NNodes, _, Graph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 > NNodes,
   NN1 is N1 + 1,
   NN2 is NN1 + 1,
   cr_gr(NN1, NN2, NNodes, Density, SoFarGraph, Graph).
cr_gr(N1, N2, NNodes, Density, SoFarGraph, Graph) :-
   N1 < NNodes,
   N2 =< NNodes,
   rand(1, 100, Rand),
   (Rand =< Density ->
      append(SoFarGraph, [N1 - N2], NewSoFarGraph) ;
      NewSoFarGraph = SoFarGraph),
   NN2 is N2 + 1,
   cr_gr(N1, NN2, NNodes, Density, NewSoFarGraph, Graph).

rand(N1, N2, R) :-
   random(R1),
   R is R1 mod (N2 - N1 + 1) + N1.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
get_node(1, [X|_], X).
get_node(Pos, [_|L], X):-
	Pos > 1,
	Pos1 is Pos - 1,
	get_node(Pos1, L, X). 

sol([], _, Clique, Clique).
sol([X|Tail], Node, R, Clique):-
	Node > 0,
	Next is Node + 1,
	( X = 1 -> append([Node], R, R1), sol(Tail, Next, R1, Clique);
		   sol(Tail, Next, R, Clique)).

make_domain(Nodes, CliqueNodes):-
	length(CliqueNodes, Nodes),
	CliqueNodes #:: 0..1.

constrain(_, _, NodeNum, NodeNum):-
	!.
constrain(Graph, CliqueNodes, NodeNum, Total):-
	Counter is NodeNum + 1, 
	constrain1(Graph, NodeNum, Counter, Total, CliqueNodes),
	constrain(Graph, CliqueNodes, Counter, Total).

constrain1(_, _, Counter, Total, _):-
	Counter > Total.
constrain1(Graph, NodeNum, Counter, Total, CliqueNodes):-
	member(NodeNum-Counter, Graph),
	NewCounter is Counter + 1,
	!,
	constrain1(Graph, NodeNum, NewCounter, Total, CliqueNodes). 
constrain1(Graph, NodeNum, Counter, Total, CliqueNodes):-
	get_node(NodeNum, CliqueNodes, Node1),
	get_node(Counter, CliqueNodes, Node2),
	Node1 + Node2 #=< 1,
	NewCounter is Counter + 1,
	constrain1(Graph, NodeNum, NewCounter, Total, CliqueNodes). 
%%%%%%%%%%%%%%%%%%%%% Maxclq Code %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

maxclq(Nodes, Density, Clique, Size):-
	create_graph(Nodes, Density, Graph),
	make_domain(Nodes, CliqueNodes),
	constrain(Graph, CliqueNodes, 1, Nodes),
	Cost #= Nodes - sum(CliqueNodes), /* If we minimize the nodes that are not in the clique we maximize the nodes that are in the clique */
	bb_min(search(CliqueNodes, 0, first_fail, indomain, complete, []), Cost, bb_options{strategy:restart}),
	writeln(CliqueNodes),
	sol(CliqueNodes, 1, [], Clique),
	Size #= sum(CliqueNodes).