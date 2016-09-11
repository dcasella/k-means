%%%%% -*- Mode: Prolog -*-

%%%%% km.pl --
%%%%% Casella Davide 793631
%%%%% Nicolini Fabio 794467



%%%%% km/3
%% True quando Clusters unifica con una lista di K gruppi di vettori
%% dell'insieme di osservazioni Observations
%
km(Observations, K, Clusters) :-
	% Controlla che il numero di osservazioni sia maggiore di K
	length(Observations, L),
	L > K,
	!,
	initialize(Observations, K, CS),
	lloyd_km(Observations, [], CS, K, ClustersMap),
	map_clusters(ClustersMap, Observations, 0, K, Clusters).
km(Observations, K, Observations) :-
	% Se il numero di osservazioni coincide con K,
	% Clusters unifica con Observations
	length(Observations, L),
	L =:= K, !.
km(_, _, _) :-
	print_message(error, "Can't compute clusters.").

%%%%% centroid/2
%% True quando Centroid unifica con il centroide dell'insieme di
%% osservazioni Observations
%
centroid(Observations, Centroid) :-
	% Somma (facendo uso di vsum) le osservazioni
	vsum_list(Observations, VSUM),
	length(Observations, L),
	% Dividi ogni coordinata del vettore generato da vsum_list per il
	% numero di osservazioni
	maplist(divide(L), VSUM, Centroid).
centroid([], []).

%%%%% vsum/3
%% True quando VSUM unifica con la somma vettoriale fra Vector1 e Vector2
%
vsum([X | Vector1], [Y | Vector2], [Z | V]) :-
	Z is X + Y,
	vsum(Vector1, Vector2, V).
vsum([], [], []).

%%%% vsub/3
%% True quando VSUB unifica con la differenza vettoriale fra Vector1 e Vector2
%
vsub([X | Vector1], [Y | Vector2], [Z | V]) :-
	Z is X - Y,
	vsub(Vector1, Vector2, V).
vsub([], [], []).

%%%% innerprod/3
%% True quando IP unifica con il prodotto scalare fra Vector1 e Vector2
%
innerprod(Vector1, Vector2, IP) :-
	prod(Vector1, Vector2, T),
	sum_list(T, IP).
innerprod([], [], []).

%%%% norm/2
%% True quando Norm unifica con la norma di Vector
%
norm(Vector, Norm) :-
	innerprod(Vector, Vector, P),
	Norm is sqrt(P).

%%%% new_vector/2
%% True quando Il predicato asserisce il predicato vector(Name, Vector)
%
new_vector(Name, Vector) :-
	atom(Name),
	vector(Vector),
	assert(vector(Name, Vector)).

%%%% initialize/3
%% True quando Centroids unifica con K vettori.
%% Questi vettori sono i centroidi iniziali dell'algoritmo K-means,
%% scelti utilizzando il metodo di Forgy;
%% metodo di Forgy: sceglie casualmente k delle osservazioni iniziali
%
initialize(_, 0, []) :- !.
initialize([], K, [[] | CS]) :-
	% Quando ci sono troppi vettori uguali in Observations
	% questo predicato permette la continuazione dell'algoritmo
	K > 0,
	!,
	J is K - 1,
	initialize([], J, CS).
initialize(Observations, K, [V | CS]) :-
	K > 0,
	length(Observations, L),
	MaxL is L-1,
	% Randomizza un indice N tra 0 e MaxL
	random_between(0, MaxL, N),
	% Estrai il vettore V di indice N
	nth0(N, Observations, V),
	% Rimuovi il vettore dalla lista Observations
	delete(Observations, V, New_Observations),
	J is K - 1,
	initialize(New_Observations, J, CS).

%%%% lloyd_km/4
%% True quando Result unifica con una lista di K gruppi di vettori
%% raggruppati per centroide
%
lloyd_km(Observations, Clusters, CS, K, Result) :-
	% Calcola la lista di gruppi di vettori ottenuta raggruppando
	% le Observations attorno ai centroidi CS(ovvero New_Clusters)
	partition(Observations, CS, New_Clusters),
	% Caso passo: i clusters calcolati nella ricorsione attuale sono diversi
	% da quelli calcolati nella ricorsione precedente
	Clusters \== New_Clusters,
	!,
	% Ricalcolo dei centroidi data la nuova lista New_Clusters
	re_centroids(New_Clusters, Observations, K, New_CS),
	lloyd_km(Observations, New_Clusters, New_CS, K, Result).
% Caso base: Clusters unifica con Result perchè la condizione
% Clusters \== New_Clusters è risultata false nella ricorsione precedente
lloyd_km(_, Clusters, _, _, Clusters).

%%%% partition/3
%% True quando Result unifica con ...
%
partition([V | Observations], CS, [NTHC | Result]) :-
	!,
	current_prolog_flag(max_tagged_integer, MAX),
	pick_centroid(V, CS, MAX, [], C),
	nth0(NTHC, CS, C),
	partition(Observations, CS, Result).
partition([], _, []).

%%%% pick_centroid/5
%% True quando Result unifica con ...
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
%% True quando CS unifica con i K centroidi ricalcolati
%% per tutte le K liste di vettori presenti in Clusters
%
re_centroids(Clusters, Observations, K, CS) :-
	map_clusters(Clusters, Observations, 0, K, MapC),
	maplist(centroid, MapC, CS).

%%%% vsum_list/2
%% True quando VSUM unifica con la somma vettoriale di tutti i vettori
%% presenti in Observatons
%
vsum_list([X | Xs], V) :-
	identity(X, IE),
	vsum_list([X | Xs], IE, V).
vsum_list([X | Xs], A, V) :-
	vsum(X, A, B),
	vsum_list(Xs, B, V).
vsum_list([], V, V).

%%%% identity/2
%% True quando IE unifica con il proprio elemento identità,
%% ovvero una lista di zeri
%
identity([_ | Xs], [0 | IE]) :-
	identity(Xs, IE).
identity([], []).

%%%% divide/3
%% True quando Quotient unifica con il quoziente della divisione algebrica
%% fra L (dividendo) e Coordinate (divisore).
%% Predicato utilizzato in centroid/3 per il calcolo del centroide
%% attraverso il predicato maplist/3
%
divide(L, Coordinate, Quotient) :-
	Quotient is Coordinate / L.

%%%% prod/3
%% True quando Prod unifica con il vettore le cui componenti sono
%% il prodotto delle componenti corrispondenti dei vettori Vector1 e Vector2
%% Predicato di appoggio di innerprod. Infatti se si sommano
%% le componenti di Prod si ottiene il prodotto scalare fra Vector1 e Vector2
%
prod([X | Vector1], [Y | Vector2], [Z | V]) :-
	Z is X * Y,
	prod(Vector1, Vector2, V).
prod([], [], []).

%%%% vector/1
%% True quando l'argomento Vector è una lista di coordinate (numeri)
%
vector([X | Vector]) :-
	number(X),
	vector(Vector).
vector([]).

%%%% map_cluster/5
%%
%
map_cluster([CL | Clusters], Observations, CL, Index, [NTHV | Result]) :-
	nth0(Index, Observations, NTHV),
	I1 is Index + 1,
	!,
	map_cluster(Clusters, Observations, CL, I1, Result).
map_cluster([_ | Clusters], Observations, CL, Index, Result) :-
	I1 is Index + 1,
	!,
	map_cluster(Clusters, Observations, CL, I1, Result).
map_cluster([], _, _, _, []).

%%%% map_clusters/5
%%
%
map_clusters(_, _, K, K, []) :- !.
map_clusters(Clusters, Observations, CL, K, [Cluster | Result]) :-
	map_cluster(Clusters, Observations, CL, 0, Cluster),
	CL1 is CL + 1,
	map_clusters(Clusters, Observations, CL1, K, Result).

%%%%% end of file -- km.pl --
