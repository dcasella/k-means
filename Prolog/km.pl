%%%%% -*- Mode: Prolog -*-

%%%%% km.pl --
%%%%% Casella Davide 793631
%%%%% Nicolini Fabio 794467



%%%%% km/3
%% True when Clusters unifies with a list of K groups of vectors
%% from the set of Observations.
%
km(Observations, K, Clusters) :-
	length(Observations, L),
	L > K,
	!,
	initialize(Observations, K, CS),
	lloyd_km(Observations, [], CS, K, ClustersMap),
	map_clusters(ClustersMap, Observations, 0, K, Clusters).
km(Observations, K, Observations) :-
	length(Observations, L),
	L =:= K, !.
km(_, _, _) :-
	print_message(error, "Can't compute clusters.").

%%%%% centroid/2
%% True when Centroid unifies with the centroid of the set Observations.
%
centroid(Observations, Centroid) :-
	vsum_list(Observations, VSUM),
	length(Observations, L),
	vdiv(VSUM, L, Centroid).
centroid([], []).

%%%%% vsum/3
%% True when VSUM unifies with the vector sum between Vector1 and Vector2.
%
vsum([X | Vector1], [Y | Vector2], [Z | V]) :-
	Z is X + Y,
	vsum(Vector1, Vector2, V).
vsum([], [], []).

%%%% vsub/3
%% True when VSUB unifies with the vector diff between Vector1 and Vector2.
%
vsub([X | Vector1], [Y | Vector2], [Z | V]) :-
	Z is X - Y,
	vsub(Vector1, Vector2, V).
vsub([], [], []).

%%%% innerprod/3
%% True when IP unifies with the dot product between Vector1 and Vector2.
%
innerprod(Vector1, Vector2, IP) :-
	vprod(Vector1, Vector2, T),
	sum_list(T, IP).
innerprod([], [], []).

%%%% norm/2
%% True when Norm unifies with the Euclidean norm of Vector.
%
norm(Vector, Norm) :-
	innerprod(Vector, Vector, P),
	Norm is sqrt(P).

%%%% new_vector/2
%% True when the predicate asserts vector(Name, Vector) correctly.
%
new_vector(Name, Vector) :-
	atom(Name),
	vector(Vector),
	assert(vector(Name, Vector)).

%%%% initialize/3
%% True when Centroids unifies with K vectors.
%% These vectors are K-means' algorithm starting centroids,
%% selected using FOrgy's method;
%% Forgy's method: randomly select K of the starting Observations.
%
initialize(_, 0, []) :- !.
initialize([], K, [[] | CS]) :-
	K > 0,
	!,
	J is K - 1,
	initialize([], J, CS).
initialize(Observations, K, [V | CS]) :-
	K > 0,
	length(Observations, L),
	MaxL is L-1,
	random_between(0, MaxL, N),
	nth0(N, Observations, V),
	delete(Observations, V, New_Observations),
	J is K - 1,
	initialize(New_Observations, J, CS).

%%%% lloyd_km/4
%% True when Result unifies with a list of K groups of vectors
%% grouped by centroid.
%
lloyd_km(Observations, Clusters, CS, K, Result) :-
	partition(Observations, CS, New_Clusters),
	Clusters \== New_Clusters,
	!,
	re_centroids(New_Clusters, Observations, K, New_CS),
	lloyd_km(Observations, New_Clusters, New_CS, K, Result).
lloyd_km(_, Clusters, _, _, Clusters).

%%%% partition/3
%% True when Result unifies with the ClustersMap which represents the list of
%% Clusters' indices to whom correspond all elements of Observations.
%
partition([V | Observations], CS, [NTHC | Result]) :-
	!,
	current_prolog_flag(max_tagged_integer, MAX),
	pick_centroid(V, CS, MAX, [], C),
	nth0(NTHC, CS, C),
	partition(Observations, CS, Result).
partition([], _, []).

%%%% pick_centroid/5
%% True when Result unifies with the centroid whose distance between itself
%% and the vector is the lowest calculated.
%
pick_centroid(V, [[] | CS], D, R, Result) :-
	!,
	pick_centroid(V, CS, D, R, Result).
pick_centroid(V, [C | CS], D, _, Result) :-
	vsub(V, C, VSUB),
	norm(VSUB, NORM),
	NORM < D,
	!,
	pick_centroid(V, CS, NORM, C, Result).
pick_centroid(V, [_ | CS], D, R, Result) :-
	!,
	pick_centroid(V, CS, D, R, Result).
pick_centroid(_, [], _, Result, Result).

%%%% re_centroids/2
%% True when CS unifies with the K centroids re-calculated for every K list
%% of vectors included in Clusters' list, which is produced as output of the
%% predicate map_clusters (input: ClustersMap' list).
%
re_centroids(ClustersMap, Observations, K, CS) :-
	map_clusters(ClustersMap, Observations, 0, K, Clusters),
	maplist(centroid, Clusters, CS).

%%%% vsum_list/2
%% True when VSUM unifies with the vector sum of all the vectors
%% included in Observatons.
%
vsum_list([X | Xs], V) :-
	identity(X, IE),
	vsum_list([X | Xs], IE, V).
vsum_list([X | Xs], A, V) :-
	vsum(X, A, B),
	vsum_list(Xs, B, V).
vsum_list([], V, V).

%%%% identity/2
%% True when IE unifies with itself's identity element, or a list of zeros.
%
identity([_ | Xs], [0 | IE]) :-
	identity(Xs, IE).
identity([], []).

%%%% vdiv/3
%% True when Result unifies with the vector whose components are
%% the division between Vector's components and L.
%% Predicate used in centroid/3 to calculate the centroid.
%
vdiv([X | Vector], L, [Q | V]) :-
	Q is X / L,
	vdiv(Vector, L, V).
vdiv([], _, []).

%%%% vprod/3
%% True when Result unifies with the vector whose components are
%% the product of Vector1 and Vector2's corresponding components.
%% Support predicate for innerprod. In fact, if we sum Prod's components
%% we obtain the dot product between Vector1 and Vector2.
%
vprod([X | Vector1], [Y | Vector2], [Z | V]) :-
	Z is X * Y,
	vprod(Vector1, Vector2, V).
vprod([], [], []).

%%%% vector/1
%% True when the argument Vector is a list of coordinate (numbers)
%
vector([X | Vector]) :-
	number(X),
	vector(Vector).
vector([]).

%%%% map_cluster/5
%% True when Result unifies with the Cluster of vectors (list of lists) of
%% index CL, given the ClustersMap (list of Clusters' indices) and
%% the starting vectors' list, Observations.
%
map_cluster([CL | ClustersMap], Observations, CL, Index, [NTHV | Result]) :-
	nth0(Index, Observations, NTHV),
	I1 is Index + 1,
	!,
	map_cluster(ClustersMap, Observations, CL, I1, Result).
map_cluster([_ | ClustersMap], Observations, CL, Index, Result) :-
	I1 is Index + 1,
	!,
	map_cluster(ClustersMap, Observations, CL, I1, Result).
map_cluster([], _, _, _, []).

%%%% map_clusters/5
%% True when Result unifies with the list of K Clusters, given
%% the ClustersMap (list of Clusters' indices) and Observations.
%
map_clusters(_, _, K, K, []) :- !.
map_clusters(ClustersMap, Observations, CL, K, [Cluster | Result]) :-
	map_cluster(ClustersMap, Observations, CL, 0, Cluster),
	CL1 is CL + 1,
	map_clusters(ClustersMap, Observations, CL1, K, Result).

%%%%% end of file -- km.pl --
