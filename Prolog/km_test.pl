assert((make_obs(0, []) :- !)).
assert((make_obs(N, [[X, Y] | Result]) :- random_between(-2, 2, X), random_between(-2, 2, Y), M is N-1, make_obs(M, Result))).
