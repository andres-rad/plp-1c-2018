% last(?L, ?U).
last([X],X).
last([X|L], Rec) :- last(L, Rec).

% reverse(?L, -L1)
reverse([], []).
reverse([X|L1], L2) :- reverse(L1, L3), append(L3, [X], L2).

% prefijo(?P, +L)
prefijo(P, L) :- append(P, S, L). 

% sufijo(?S, +L)
sufijo(S, L) :- append(P, S, L).

% sublista(?S, +L)
sublista(S, L) :- prefijo(P, L), sufijo(Su, L), append(P, S, L1), append(L1, Su, L).

% pertenece(?X, +L)
pertenece(X, L) :- append(L1, [X|L2], L).
