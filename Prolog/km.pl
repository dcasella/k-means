% Casella Davide 793631
% Nicolini Fabio 794467

% Predicati principali

km(_Observations, _K, _Clusters).

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

vector([X | Vector]) :-
	number(X),
	vector(Vector).
vector([]).

prod([X | Vector1], [Y | Vector2], [Z | V]) :-
	Z is X * Y,
	prod(Vector1, Vector2, V).
prod([], [], []).

vsum_list([X | Xs], V) :-
	identity(X, IE),
	vsum_list([X | Xs], IE, V).
vsum_list([X | Xs], A, V) :-
	vsum(X, A, LUL),
	vsum_list(Xs, LUL, V).
vsum_list([], V, V).

identity([_ | Xs], [0 | IE]) :-
	identity(Xs, IE).
identity([], []).

divide(L, Coordinate, Result) :-
	Result is Coordinate / L.
	