% Casella Davide 793631
% Nicolini Fabio 794467

% Predicati principali

km(Observations, K, Clusters) :-
	length(Observations, L),
	L >= K,
	!,
	initialize(Observations, K, CS),
	km_r(Observations, [], CS, Clusters).
km(_, _, _) :-
	print_message(error, 42).

centroid(Observations, Centroid) :-
	vsum_list(Observations, VSUM),
	length(Observations, L),
	maplist(divide(L), VSUM, Centroid).

vsum([X | Vector1], [Y | Vector2], [Z | V]) :-
	Z is X + Y,
	vsum(Vector1, Vector2, V).
vsum([], [], []).	

vsub([X | Vector1], [Y | Vector2], [Z | V]) :-
	Z is X - Y,
	vsub(Vector1, Vector2, V).
vsub([], [], []).	

innerprod(Vector1, Vector2, R) :-
	prod(Vector1, Vector2, T),
	sum_list(T, R).
innerprod([], [], []).	

norm(Vector, N) :-
	innerprod(Vector, Vector, P),
	N is sqrt(P).

new_vector(Name, Vector) :-
	atom(Name),
	vector(Vector),
	assert(vector(Name, Vector)).

% Predicati ausiliari

initialize(Observations, K, [V | CS]) :-
	K > 0,
	!,
	length(Observations, L),
	MaxL is L-1,
	random_between(0, MaxL, N),
	nth0(N, Observations, V),
	delete(Observations, V, New_Observations),
	J is K-1,
	initialize(New_Observations, J, CS).
initialize(_, 0, []).

km_r(Observations, Clusters, CS, Result) :-
	partition(Observations, CS, New_Clusters),
	Clusters \== New_Clusters,
	!,
	re_centroids(New_Clusters, New_CS),
	km_r(Observations, New_Clusters, New_CS, Result).
km_r(_, Clusters, _, Clusters).

partition(Observations, CS, Clusters) :-
	partition_n(Observations, CS, PN),
	append(PN, FPN),
	sort(FPN, New_FPN),
	partition_a(New_FPN, [], PA),
	sort(PA, New_PA),
	partition_r(New_PA, [], [], Clusters).

partition_n(Observations, [C | CS], [NR | Result]) :-
	!,
	norm_r(Observations, C, NR),
	partition_n(Observations, CS, Result).
partition_n(_, [], []).

norm_r([V | Observations], C, [[NORM, C, V] | Result]) :-
	!,
	vsub(V, C, VSUB),
	norm(VSUB, NORM),
	norm_r(Observations, C, Result).
norm_r([], _, []).

partition_a([[_N, C, V] | Observations], [], [[C, V] | Result]) :-
	!,
	partition_a(Observations, [V], Result).
partition_a([[_N, _C, V] | Observations], Acc, Result) :-
	member(V, Acc),
	!,
	partition_a(Observations, Acc, Result).
partition_a([[_N, C, V] | Observations], Acc, [[C, V] | Result]) :-
	\+member(V, Acc),
	!,
	partition_a(Observations, [V | Acc], Result).
partition_a([], _, []).

partition_r([[C, V] | Observations], _, [], Result) :-
	!,
	partition_r(Observations, C, [V], Result).
partition_r([[C, V] | Observations], C, ACC2, Result) :-
	!,
	partition_r(Observations, C, [V | ACC2], Result).
partition_r([[C, V] | Observations], _, ACC2, [ACC2 | Result]) :-
	!,
	partition_r(Observations, C, [V], Result).
partition_r([], _, ACC2, [ACC2]).

re_centroids(Clusters, CS) :-
	maplist(centroid, Clusters, CS).

vsum_list([X | Xs], V) :-
	identity(X, IE),
	vsum_list([X | Xs], IE, V).
vsum_list([X | Xs], A, V) :-
	vsum(X, A, B),
	vsum_list(Xs, B, V).
vsum_list([], V, V).

identity([_ | Xs], [0 | IE]) :-
	identity(Xs, IE).
identity([], []).

divide(L, Coordinate, Result) :-
	Result is Coordinate / L.

prod([X | Vector1], [Y | Vector2], [Z | V]) :-
	Z is X * Y,
	prod(Vector1, Vector2, V).
prod([], [], []).

vector([X | Vector]) :-
	number(X),
	vector(Vector).
vector([]).
