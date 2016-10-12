assert((make_obs(0, []) :- !)).

assert((make_obs(N, [[X, Y, Z] | Result]) :- random_between(-50, 50, X), random_between(-50, 50, Y), random_between(-50, 50, Z), M is N-1, make_obs(M, Result))).
