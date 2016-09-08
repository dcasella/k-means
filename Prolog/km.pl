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
	km_r(Observations, [], CS, Clusters).
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
initialize(Observations, K, [V | CS]) :-
	K > 0,
	!,
	length(Observations, L),
	MaxL is L-1,
	% Randomizza un indice N tra 0 e MaxL
	random_between(0, MaxL, N),
	% Estrai il vettore V di indice N
	nth0(N, Observations, V),
	% Rimuovi il vettore dalla lista Observations
	delete(Observations, V, New_Observations),
	J is K-1,
	initialize(New_Observations, J, CS).
% Caso base: K è pari a 0
initialize(_, 0, []).

%%%% km_r/4
%% True quando Result unifica con una lista di K gruppi di vettori
%% raggruppati per centroide
%
km_r(Observations, Clusters, CS, Result) :-
	% Calcola la lista di gruppi di vettori ottenuta raggruppando
	% le Observations attorno ai centroidi CS(ovvero New_Clusters)
	partition(Observations, CS, New_Clusters),
	% Caso passo: i clusters calcolati nella ricorsione attuale sono diversi
	% da quelli calcolati nella ricorsione precedente
	Clusters \== New_Clusters,
	!,
	% Ricalcolo dei centroidi data la nuova lista New_Clusters
	re_centroids(New_Clusters, New_CS),
	km_r(Observations, New_Clusters, New_CS, Result).
% Caso base: Clusters unifica con Result perchè la condizione
% Clusters \== New_Clusters è risultata false nella ricorsione precedente
km_r(_, Clusters, _, Clusters).

%%%% partition/3
%% True quando Clusters unifica con la lista di K gruppi di vettori
%% raggruppati intorno ai centroidi CS
%
partition(Observations, CS, Clusters) :-
	partition_n(Observations, CS, PN),
	sort(PN, SPN),
	partition_r(SPN, [], [], Clusters).

%%%% partition_n/3
%% True quando Result unifica con ...
%
partition_n([V | Observations], CS, [NR | Result]) :-
	!,
	current_prolog_flag(max_tagged_integer, MAX),
	norm_r(V, CS, MAX, [], NR),
	partition_n(Observations, CS, Result).
partition_n([], _, []).

%%%% norm_r/5
%% True quando Result unifica con ...
%
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

%%%% partition_n/4
%% True quando Result unifica con ...
%
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

%%%% re_centroids/2
%% True quando CS unifica con i K centroidi ricalcolati
%% per tutte le K liste di vettori presenti in Clusters
%
re_centroids(Clusters, CS) :-
	maplist(centroid, Clusters, CS).

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

%%%%% end of file -- km.pl --
