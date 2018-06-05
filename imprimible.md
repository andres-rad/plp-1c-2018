```prolog
symbol(a).
symbol(b).
symbol(c).

% Algunas regex de ejemplo

regexEj(1, a). % a
regexEj(2, or(a, b)). % a|b
regexEj(3, concat(E1, E2)) :- regexEj(1, E1), regexEj(2, E2). % a(a|b)
regexEj(4, star(E2)) :- regexEj(2, E2). % (a(a|b))*
regexEj(5, or(star(E1), E4)) :- regexEj(1, E1), regexEj(4, E4). % (a*|(a(a|b))*)
regexEj(6, star(or(a, ab))). %(a|ab)*
regexEj(7, concat(or(a, concat(a,b)), or(b, empty))). %(a|ab)(b|)
regexEj(8, concat(star(a), star(b))). %a*b*
regexEj(9, star(or(star(a), star(b)))).

% Ejercicio 1: tieneEstrella(+RegEx)

tieneEstrella(concat(R1,R2)) :- tieneEstrella(R1), ! ; tieneEstrella(R2), !.
tieneEstrella(or(R1,R2)) :- tieneEstrella(R1), ! ; tieneEstrella(R2), !.
tieneEstrella(star(_)).
```

```control
?- tieneEstrella(a).
false.

?- tieneEstrella(concat(a,or(b, star(a)))).
true.
```

```prolog
% Ejercicio 2: longitudMaxima(+RegEx, -Length)
longitudMaxima(empty,0).
longitudMaxima(RegEx,1) :- symbol(RegEx).
longitudMaxima(concat(R1,R2),N) :- longitudMaxima(R1,M), longitudMaxima(R2,O), N is M+O.
longitudMaxima(or(R1,R2),N) :- longitudMaxima(R1,M), longitudMaxima(R2,O), N is max(M,O).
```

```control
?- longitudMaxima(a, X).
X = 1.

?- longitudMaxima(concat(a, star(concat(a, b))), X).
false.

?- longitudMaxima(or(concat(a, concat(b, c)), or(b, c)), X).
X = 3.
```

```prolog
%desde(+X, -Y) genera todos los numeros desde X
desde(X,X).
desde(X,Y) :- N is X+1, desde(N, Y).

%cadena_acotada(?Cad, +N) genera todas las cadenas de símbolos de longitud N
cadena_acotada([], 0).
cadena_acotada([X|L], N) :- N > 0, symbol(X), M is N-1, cadena_acotada(L,M).

% Ejercicio 3: cadena(?Cadena)
cadena([]).
cadena([X|L]) :- ground([X|L]), symbol(X), cadena(L).
cadena([X|L]) :- not(ground([X|L])), desde(1,I), cadena_acotada([X|L], I).
```

```control
?- cadena([a,b,c,a,c,d]).
false.

?- cadena([a,b,c,b]).
true.

?- cadena(X).
X = [];
X = [a];
X = [b];
X = [c];
X = [a,a];
X = [a,b]
...
```

```prolog
% Ejercicio 4: match_inst(+Cadena, +RegEx)

match_inst([], empty).
match_inst([RegEx], RegEx) :- symbol(RegEx).

match_inst(L, concat(R1,R2)) :- append(A, B, L), match_inst(A, R1), match_inst(B, R2), !.
match_inst(L, or(R1,R2)) :- match_inst(L, R1), ! ; match_inst(L, R2), !.

match_inst([], star(_)).
match_inst([X|L], star(RegEx)) :- append([A1|As], B, [X|L]), match_inst([A1|As], RegEx),
 								 match_inst(B, star(RegEx)), !.
```

```control
?- regexEj(_, Y), match_inst([a,b,a,b], Y).
Y = star(or(a,b));
Y = or(star(a), star(or(a, b)));
Y = star(or(star(a), star(b))).
```


```prolog
% Ejercicio 5: match(?Cadena, +RegEx)

match(Cad, RegEx) :-  longitudMaxima(RegEx, M), between(0, M, N),
					  cadena_acotada(Cad, N), match_inst(Cad, RegEx).
match(Cad, RegEx) :-  not(longitudMaxima(RegEx,_)), cadena(Cad), match_inst(Cad, RegEx).
```

```control
?- match(X, or(concat(a,b), concat(b,c))).
X = [a, b];
false.

?- match([a,b,c,a,b,c], star(concat(a, concat(b,c)))).
true.

?- match(X, or(star(a), star(b))).
X = [];
X = [a];
X = [b];
X = [a, a];
X = [a, b];
X = [b, a];
X = [b, b];
...
```

```prolog
% Ejercicio 6: diferencia(?Cadena, +RegEx, +RegEx)
diferencia(Cad, RegEx1, RegEx2) :- match(Cad, RegEx1), not(match(Cad, RegEx2)).
```

```control
?- diferencia(X, star(concat(a,b)), concat(a,b)).
X = [];
X = [a,b,a,b];
X = [a,b,a,b,a,b];
...

?- diferencia([a,b,c], star(concat(a, conat(b, c))), concat(a, b)).
true.
```

```prolog
%prefijo(?P, ?L) P
prefijo(P, L) :- append(P, _, L).

%prefijo_que_matchea(P?, Cad?, +RegEx)
prefijo_que_matchea(P, Cad, RegEx) :- prefijo(P, Cad), match_inst(P, RegEx).

%prefijo_que_matchea(P?, Cad?, +RegEx)
prefijo_que_matchea_y_mayor(P, Cad, RegEx) :- prefijo_que_matchea(P2, Cad, RegEx),
											  length(P, L1), length(P2, L2), L2 > L1.

% Ejercicio 7: prefijoMaximo(?Prefijo, +Cadena, +RegEx)
prefijoMaximo(PMax, Cad, RegEx) :- prefijo_que_matchea(PMax, Cad, RegEx),
  								 not(prefijo_que_matchea_y_mayor(PMax, Cad, RegEx)).

```

```control
?- prefijoMaximo(P, [a,b,a,a,c], or(concat(a,b), concat(a, concat(b, star(a))))).
P = [a,b,a,a];
false.

?- prefijoMaximo([], [a,b], or(empty,a)).
false.
```

```prolog
% Ejercicio 8: reemplazar(+Cad, +RegEx, +T, -R)
sufijo(S, L) :- append(_, S, L).

sublista([],L):- is_list(L).
sublista([H|T],L):- sufijo(S, L), prefijo([H|T], S) .

append3([],[],L,L).
append3([],[H|T],L,[H|R]) :- append(T,L,R).
append3([H|T],L2,L3,[H|R]) :- append3(T,L2,L3,R).

subCadenaMaxima(ScMax, Cad, RegEx) :- sublista_que_matchea(ScMax, Cad, RegEx),
	  								not(sublista_que_matchea_y_mayor(ScMax, Cad, RegEx)).

sublista_que_matchea(ScMax, Cad, RegEx) :- sublista(ScMax, Cad), match_inst(ScMax, RegEx).

sublista_que_matchea_y_mayor(ScMax, Cad, RegEx) :- sublista_que_matchea(ScMax2, Cad, RegEx),
     										 	 length(ScMax, L1), length(ScMax2, L2), L2 > L1.

%buscar_particion(+S, +Cad, -Front, -End)
%busca S en Cad y devuelve lo que está antes en Front y lo que está despues en End.
buscar_particion(S, Cad, Front, End) :- append3(Front, S, End, Cad).

%reemplazarReps(+Cad, +RegEx, +T, -R)
%Esta versión reemplaza primero la subcadena más larga que acepte el regex,
%no tiene por que ser la primer subcadena reemplazable,
%si parte la cadena en dos, continua reemplazando recursivamente en las dos subcadenas
%reestantes

%Observación, los posibles repetidos que puede dar son debido a
%la entrada y segun enunciado no hace falta filtrarlos. Por ej:
%?- reemplazar([a,b], or(star(a),star(b)), [1], X ).
%X = [1, 1] ; %Empezando con subCadenaMaxima a
%X = [1, 1] ; %Empezando con subCadenaMaxima b
%false.


reemplazar(Cad, RegEx, _, Cad):- not(subCadenaMaxima(_, Cad, RegEx)).
reemplazar(Cad, RegEx, _, Cad) :- subCadenaMaxima([], Cad, RegEx).
reemplazar(Cad, RegEx, T, R) :- subCadenaMaxima([S|Ss], Cad, RegEx),
								buscar_particion([S|Ss], Cad, Front_i, End_i),
								reemplazar(Front_i, RegEx, T, R1),
								reemplazar(End_i, RegEx, T, R2),
								append3(R1,T,R2,R).

%reemplazar(+Cad, +RegEx, +T, -R) elimina resultados repetidos.
reemplazar(Cad, RegEx, T, R) :- setof(X, reemplazar(Cad, RegEx, T, X), Set), member(R, Set).

%reemplazarLineal(+Cad, +RegEx, +T, -R)
%Esta versión de reemplazar empieza mirando la cadena de izquierda a derecha,
%una vez que encuentra una subcadena que matchea, intenta extender esa cadena
%lo más posible y realiza ese reemplazo, una vez que termina sigue con el resto de
%la cadena.
reemplazarLineal([], _, _, []).
reemplazarLineal([C|CS], RegEx, T, Res) :- not(prefijoMaximo(_, [C|CS], R)),
									  	 reemplazarLineal(CS, R, T, Rec),
		       						    append([C], Rec, Res).
reemplazarLineal([C|CS], RegEx, T, Res) :- prefijoMaximo([], [C|CS], R),
						       			reemplazarLineal(C, RegEx, T, Rec),
							        	   append([C], Rec, Res).
reemplazarLineal([C|CS], RegEx, T, Res) :- prefijoMaximo([P|PS], [C|CS], R), append([P|PS], Y, [C|CS]),
								 		  reemplazarLineal(Y, R, T, Rec), append(T, Rec, Res).
```

```control
reemplazar([a,b,a,a,a], or(concat(a,concat(b,star(b))), concat(b, star(a))), [1], X).
X = [a, 1].

reemplazarLineal([a,b,a,a,a], or(concat(a,concat(b,star(b))), concat(b, star(a))), [1], X).
X = [1, a, a, a].

reemplazar([a,b,c,b,c], concat(b, c), [as], X).
X = [a, as, as].

reemplazarLineal([a,b,c,b,c], concat(b,c), [as], X).
X = [a, as, as].
```
