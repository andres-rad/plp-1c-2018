% aplanar(+Xs, -Ys)
aplanar([], []) :- !.
aplanar([X|L],A) :- aplanar(X, B), aplanar(L, C), append(B, C, A).
aplanar(L, [L]).
