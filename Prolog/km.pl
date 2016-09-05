% Casella Davide 793631
% Nicolini Fabio 794467

% Predicati principali

% km/3
% Parametro Observations, lista di vettori (ovvero liste).
% Parametro K, numero di clusters da generare.
% Ritorna K clusters dell'insieme di osservazioni Observations.
km(Observations, K, Clusters) :-
	% Controlla che il numero di osservazioni sia maggiore di K
	length(Observations, L),
	L > K,
	!,
	initialize(Observations, K, CS),
	km_r(Observations, [], CS, Clusters).
km(Observations, K, Observations) :-
	% Se il numero di osservazioni coincide con K, ritorna Observations
	length(Observations, L),
	L =:= K, !.
km(_, _, _) :-
	% Errore: impossibile computare i clusters
	print_message(error, "Can't compute clusters.").

% centroid/2
% Parametro Observations, lista di vettori (ovvero liste).
% Calcola il centroide dell'insieme di osservazioni Observations.
centroid(Observations, Centroid) :-
	% Somma (facendo uso di vsum) le osservazioni
	vsum_list(Observations, VSUM),
	length(Observations, L),
	% Dividi ogni coordinata del vettore generato da vsum_list per il
	% numero di osservazioni
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
	sort(PN, SPN),
	partition_r(SPN, [], [], Clusters).

partition_n([V | Observations], CS, [NR | Result]) :-
	!,
	current_prolog_flag(max_tagged_integer, MAX),
	norm_r(V, CS, MAX, [], NR),
	partition_n(Observations, CS, Result).
partition_n([], _, []).

norm_r(V, [C | CS], D, _, Result) :-
	vsub(V, C, VSUB),
	norm(VSUB, NORM),
	NORM < D,
	!,
	norm_r(V, CS, NORM, [C, V], Result).
norm_r(V, [_ | CS], D, R, Result) :-
	!,
	norm_r(V, CS, D, R, Result).
norm_r(_, [], _, Result, Result).

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
