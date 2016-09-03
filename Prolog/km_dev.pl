make_obs(0, []) :- !.
make_obs(N, [[X, Y] | Result]) :-
	random_between(-50, 50, X),
	random_between(-50, 50, Y),
	M is N-1,
	make_obs(M, Result).
