% Casella Davide 793631
% Nicolini Fabio 794467

% Predicati principali

%! km(+Observations, +K, -Clusters)
% Argomento Observations, lista di vettori (ovvero liste)
% Argomento K, numero di clusters da generare
% Clusters unifica con una lista di  K gruppi di vettori dell'insieme di 
% osservazioni Observations
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

%! centroid(+Observations, -Centroid)
% Argomento Observations, lista di vettori (ovvero liste di coordinate)
% Centroid unifica con il centroide dell'insieme di osservazioni Observations
centroid(Observations, Centroid) :-
	% Somma (facendo uso di vsum) le osservazioni
	vsum_list(Observations, VSUM),
	length(Observations, L),
	% Dividi ogni coordinata del vettore generato da vsum_list per il
	% numero di osservazioni
	maplist(divide(L), VSUM, Centroid).

%! vsum(+Vector1, +Vector2, -VSUM)
% Argomento Vector1, vettore (lista di coordinate)
% Argomento Vector2, vettore (lista di coordinate)
% VSUM unifica con la somma vettoriale fra Vector1 e Vector2 
vsum([X | Vector1], [Y | Vector2], [Z | V]) :-
	Z is X + Y,
	vsum(Vector1, Vector2, V).
vsum([], [], []).

%! vsub(+Vector1, +Vector2, VSUB)
% Argomento Vector1, vettore (lista di coordinate)
% Argomento Vector2, vettore (lista di coordinate)
% VSUM unifica con la differenza vettoriale fra Vector1 e Vector2
vsub([X | Vector1], [Y | Vector2], [Z | V]) :-
	Z is X - Y,
	vsub(Vector1, Vector2, V).
vsub([], [], []).

%! innerprod(+Vector1, +Vector2, -R)
% Argomento Vector1, vettore (lista di coordinate)
% Argomento Vector2, vettore (lista di coordinate)
% R unifica con il prodotto scalare fra Vector1 e Vector2
innerprod(Vector1, Vector2, R) :-
	prod(Vector1, Vector2, T),
	sum_list(T, R).
innerprod([], [], []).

%! norm(+Vector, -N)
% Argomento Vector, vettore (lista di coordinate)
% N unifica con la norma di Vector
norm(Vector, N) :-
	innerprod(Vector, Vector, P),
	N is sqrt(P).

%! new_vector(+Name, +Vector)
% Argomento Name, un atomo prolog
% Argomento Vector, vettore (lista di coordinate)
% Il predicato asserisce il predicato vector(Name, Vector)
new_vector(Name, Vector) :-
	atom(Name),
	vector(Vector),
	assert(vector(Name, Vector)).

% Predicati ausiliari

%! initialize(+Observations, +K, -Centroids)
% Argomento Observations, lista di vettori (ovvero liste)
% Argomento K, numero di centroidi da generare
% Centroids unifica con K vettori. Questi vettori sono i centroidi iniziali
% dell'algoritmo K-means, scelti utilizzando il metodo di Forgy;
% metodo di Forgy: sceglie casualmente k delle osservazioni iniziali
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

%! km_r(+Observations, +Clusters, +Cs, -Result)
% Argomento Observations, lista di vettori (ovvero liste)
% Argomento Clusters, lista di gruppi di vettori calcolati nella ricorsione
% precedente (NIL durante la prima chiamata).
% Argomento CS, lista di centroidi
% Result unifica con la lista di K gruppi di vettori (ovvero di liste),
% raggruppati per centroidi
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
% Caso base: i clusters calcolati nella ricorsione attuale sono
% uguali a quelli calcolati nella ricorsione precedente	
km_r(_, Clusters, _, Clusters).

% partition(+Observations, +CS, -Clusters)
% Argomento Observations, lista di vettori (ovvero liste)
% Argomento CS, lista di centroidi
% Clusters unifica con la lista di k gruppi di vettori, raggruppati intorno
% ai centroidi CS
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

%! re_centroids(+Clusters, -CS)
% Argomento Clusters, lista di liste di vettori(ovvero liste)
% CS unifica con i K centroidi ricalcolati per tutte le K liste di vettori
% presenti in Clusters
re_centroids(Clusters, CS) :-
	maplist(centroid, Clusters, CS).

%! vsum_list(+Observations, -VSUM)
% Argomento Observations, lista di vettori
% VSUM unifica con la somma vettoriale di tutti i vettori
% presenti in Observatons
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

%! divide(+L, +Coordinate, -Result)
% Argomento L, un numero
% Argomento Coordinate, un numero
% Result unifica con il quoziente della divisione(algebrica) fra L(dividendo) e
% Coordinate(divisore). Predicato utilizzato in centroid/3 per il calcolo del
% centroide attraverso il predicato maplist/3
divide(L, Coordinate, Result) :-
	Result is Coordinate / L.

%! prod(+Vector1, +Vector2, -V)
% Argomento Vector1, un vettore (lista di coordinate)
% Argomento Vector2, un vettore (lista di coordinate)
% V unifica con il vettore le cui componenti sono il prodotto delle componenti
% corrispondenti dei vettori Vector1 e Vector2
% E' un predicato di appoggio di innerprod. Infatti se si sommano le 
% componenti di V si ottiene il prodotto scalare fra Vector1 e Vector2 
prod([X | Vector1], [Y | Vector2], [Z | V]) :-
	Z is X * Y,
	prod(Vector1, Vector2, V).
prod([], [], []).

%! vector(+Vector)
% Argomento Vector, un vettore(lista di coordinate)
% Il predicato restituisce true se l'argomento Vector
% è una lista di coordinate(numeri), falso altrimenti
vector([X | Vector]) :-
	number(X),
	vector(Vector).
vector([]).
