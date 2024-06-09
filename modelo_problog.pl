% Importing necessary standard library modules
:- use_module(library(lists)).
count_aux(Lista, N, Acc, Acc) :-
	 \+ select(N, Lista, _).
count_aux(Lista, N, Acc, Count) :-
	select(N, Lista, Resto), NewAcc is Acc + 1, 
	count_aux(Resto, N, NewAcc, Count).
count(Lista, N, Count) :-
	count_aux(Lista, N, 0, Count).

sublist([], _).
sublist([H|T], L) :-
	member(H, L), 
	select(H, L, Rest), 
	sublist(T, Rest).

dado(1).
dado(2).
dado(3).
dado(4).
dado(5).

categorias([aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]).

1/6::elegir(X, 1) ; 1/6::elegir(X, 2) ; 1/6::elegir(X, 3) ; 1/6::elegir(X, 4) ; 1/6::elegir(X, 5) ; 1/6::elegir(X, 6).
dado(X, Y) :-
	dado(X), 
	elegir(X, Y).
calcular_tirada([X1, X2, X3, X4, X5]) :-
	dado(1, X1), 
	dado(2, X2), 
	dado(3, X3), 
	dado(4, X4), 
	dado(5, X5).

% Es verdadero cuando Patron es el patron que maximiza las probabilidades de obtener Categoria para Dados
% calcular_patron(+Dados, -Patron, +Categoria)

calcular_patron_superior_aux([], [], _, _).
calcular_patron_superior_aux([NCategoria|RestoDados], [0|RestoPatron], NCategoria, NDado) :-
	NDadoNew is NDado + 1, 
	calcular_patron_superior_aux(RestoDados, RestoPatron, NCategoria, NDadoNew).
calcular_patron_superior_aux([DadoActual|RestoDados], [1|RestoPatron], NCategoria, NDado) :-
	DadoActual \= NCategoria, 
	dado(NDado, NCategoria), NDadoNew is NDado + 1, 
	calcular_patron_superior_aux(RestoDados, RestoPatron, NCategoria, NDadoNew).


% Three of a kind
calcular_three_of_a_kind_aux([], [], _, _, _, _).
% Caso 1: No encontre los 3 dados todavia y me encuentro un dado
calcular_three_of_a_kind_aux([Dado|Restantes], [0|Patron], Dado, NDado, Acc, Faltantes) :-
	Acc < 3, AccNew is Acc + 1, NDadoNew is NDado + 1, 
	calcular_three_of_a_kind_aux(Restantes, Patron, Dado, NDadoNew, AccNew, Faltantes).
% Caso 2: No encontre los 3 dados y no me encontre un dado
% Caso 2.1: Faltan dados de tipo N: re roleo el dado actual
calcular_three_of_a_kind_aux([Dado|Restantes], [1|Patron], N, NDado, Acc, Faltantes) :-
	Acc < 3, Faltantes > 0, Dado \= N, NDadoNew is NDado + 1, AccNew is Acc + 1, FaltantesNew is Faltantes - 1, 
	dado(NDado, N), 
	calcular_three_of_a_kind_aux(Restantes, Patron, N, NDadoNew, AccNew, FaltantesNew).
% Caso 2.2: No faltan dados de tipo N: re roleo el dado actual si es menor o igual a 3
calcular_three_of_a_kind_aux([Dado|Restantes], [1|Patron], N, NDado, Acc, Faltantes) :-
	Acc < 3, Faltantes =< 0, Dado =< 3, Dado \= N, NDadoNew is NDado + 1, 
	calcular_three_of_a_kind_aux(Restantes, Patron, N, NDadoNew, Acc, Faltantes).
% Caso 2.3: No faltan dados de tipo N: no re roleo el dado actual si es mayor a 3
calcular_three_of_a_kind_aux([Dado|Restantes], [0|Patron], N, NDado, Acc, Faltantes) :-
	Acc < 3, Faltantes =< 0, Dado > 3, Dado \= N, NDadoNew is NDado + 1,  
	calcular_three_of_a_kind_aux(Restantes, Patron, N, NDadoNew, Acc, Faltantes).
% Caso 3: Ya encontre los 3 dados y el actual me da buen puntaje
calcular_three_of_a_kind_aux([Dado|Restantes], [0|Patron], N, NDado, Acc, Faltantes) :-
	Acc >= 3, Dado > 3, NDadoNew is NDado + 1, 
	calcular_three_of_a_kind_aux(Restantes, Patron, N, NDadoNew, Acc, Faltantes).
% Caso 4: Ya encontre los 3 dados y el actual no me da buen puntaje
calcular_three_of_a_kind_aux([Dado|Restantes], [1|Patron], N, NDado, Acc, Faltantes) :-
	Acc >= 3, Dado =< 3, NDadoNew is NDado + 1, 
	calcular_three_of_a_kind_aux(Restantes, Patron, N, NDadoNew, Acc, Faltantes).
calcular_three_of_a_kind(Dados, Patron) :-
	member(N, [1, 2, 3, 4, 5, 6]),
	count(Dados, N, Count), Faltantes is 3 - Count, 
	calcular_three_of_a_kind_aux(Dados, Patron, N, 1, 0, Faltantes).

calcular_four_of_a_kind_aux([], [], _, _, _, _).
% Caso 1: No encontre los 4 dados todavia y me encuentro un dado
calcular_four_of_a_kind_aux([Dado|Restantes], [0|Patron], Dado, NDado, Acc, Faltantes) :-
	Acc < 4, AccNew is Acc + 1, NDadoNew is NDado + 1, 
	calcular_four_of_a_kind_aux(Restantes, Patron, Dado, NDadoNew, AccNew, Faltantes).
% Caso 2: No encontre los 4 dados y no me encontre un dado
% Caso 2.1: Faltan dados de tipo N: re roleo el dado actual
calcular_four_of_a_kind_aux([Dado|Restantes], [1|Patron], N, NDado, Acc, Faltantes) :-
	Acc < 4, Faltantes > 0, Dado \= N, NDadoNew is NDado + 1, AccNew is Acc + 1, FaltantesNew is Faltantes - 1, 
	dado(NDado, N), 
	calcular_four_of_a_kind_aux(Restantes, Patron, N, NDadoNew, AccNew, FaltantesNew).
% Caso 2.2: No faltan dados de tipo N: re roleo el dado actual si es menor o igual a 3
calcular_four_of_a_kind_aux([Dado|Restantes], [1|Patron], N, NDado, Acc, Faltantes) :-
	Acc < 4, Faltantes =< 0, Dado =< 3, Dado \= N, NDadoNew is NDado + 1,
	calcular_four_of_a_kind_aux(Restantes, Patron, N, NDadoNew, Acc, Faltantes).
% Caso 2.3: No faltan dados de tipo N: no re roleo el dado actual si es mayor a 3
calcular_four_of_a_kind_aux([Dado|Restantes], [0|Patron], N, NDado, Acc, Faltantes) :-
	Acc < 4, Faltantes =< 0, Dado > 3, Dado \= N, NDadoNew is NDado + 1,
	calcular_four_of_a_kind_aux(Restantes, Patron, N, NDadoNew, Acc, Faltantes).
% Caso 3: Ya encontre los 4 dados y el actual me da buen puntaje
calcular_four_of_a_kind_aux([Dado|Restantes], [0|Patron], N, NDado, Acc, Faltantes) :-
	Acc >= 4, Dado > 3, NDadoNew is NDado + 1, 
	calcular_four_of_a_kind_aux(Restantes, Patron, N, NDadoNew, Acc, Faltantes).
% Caso 4: Ya encontre los 4 dados y el actual no me da buen puntaje
calcular_four_of_a_kind_aux([Dado|Restantes], [1|Patron], N, NDado, Acc, Faltantes) :-
	Acc >= 4, Dado =< 3, NDadoNew is NDado + 1,
	calcular_four_of_a_kind_aux(Restantes, Patron, N, NDadoNew, Acc, Faltantes).

calcular_four_of_a_kind(Dados, Patron) :-
	writeln('Four of a kind'),
	member(N, [1, 2, 3, 4, 5, 6]), 
	count(Dados, N, Count), 
	writeln(N), Faltantes is 4 - Count, 
	calcular_four_of_a_kind_aux(Dados, Patron, N, 1, 0, Faltantes), 
	writeln(Patron).

calcular_full_house_aux([], [], N, M, _).
calcular_full_house_aux([N|Restantes], [0|Patron], N, M, NDado) :-
	NDadoNew is NDado + 1, 
	calcular_full_house_aux(Restantes, Patron, N, M, NDadoNew).
calcular_full_house_aux([M|Restantes], [0|Patron], N, M, NDado) :-
	NDadoNew is NDado + 1, 
	calcular_full_house_aux(Restantes, Patron, N, M, NDadoNew).
calcular_full_house_aux([Dado|Restantes], [1|Patron], N, M, NDado) :-
	Dado \= N, Dado \= M, NDadoNew is NDado + 1, 
	dado(NDado, M), 
	calcular_full_house_aux(Restantes, Patron, N, M, NDadoNew).

% Si tenemos 3 de un dado y 2 de otro, entonces ya tenemos un full house
calcular_patron_full_house(Dados, Patron) :-
	count(Dados, N, 3), 
	count(Dados, M, 2), N \= M, 
	calcular_full_house_aux(Dados, Patron, N, M, 1).
% Si tenemos 3 de un dado, re roleamos uno de los otros 2
calcular_patron_full_house(Dados, Patron) :-
	count(Dados, N, 3), 
	count(Dados, M, 1), N \= M, 
	calcular_full_house_aux(Dados, Patron, N, M, 1).
% Si tenemos 2 y 2, re roleamos el quinto
calcular_patron_full_house(Dados, Patron) :-
	count(Dados, N, 2), 
	count(Dados, M, 2), N \= M, 
	calcular_full_house_aux(Dados, Patron, N, M, 1).
% Si tenemos 2 re roleamos 2 de los otros 3
calcular_patron_full_house(Dados, Patron) :-
	count(Dados, N, 2), 
	count(Dados, M, 1), N \= M, 
	calcular_full_house_aux(Dados, Patron, N, M, 1).

small_straight([1,2,3,4]).
small_straight([2,3,4,5]).
small_straight([3,4,5,6]).

calcular_small_straight_aux([], _, [], _).
calcular_small_straight_aux([Dado|Restantes], RestoStraight, [0|Patron], NDado) :-
	select(Dado, RestoStraight, NewRestoStraight), NewNDado is NDado + 1, 
	calcular_small_straight_aux(Restantes, NewRestoStraight, Patron, NewNDado).
calcular_small_straight_aux([Dado|Restantes], RestoStraight, [1|Patron], NDado) :-
	 \+ select(Dado, RestoStraight, _), 
	dado(NDado, Dado), NewNDado is NDado + 1, 
	calcular_small_straight_aux(Restantes, RestoStraight, Patron, NewNDado).
calcular_small_straight(Dados, [0, 0, 0, 0, 0]) :-
	small_straight(Straight), 
	sublist(Straight, Dados).
calcular_small_straight(Dados, Patron) :-
	small_straight(Straight), 
	calcular_small_straight_aux(Dados, Straight, Patron, 1).

large_straight([1,2,3,4,5]).
large_straight([2,3,4,5,6]).

calcular_large_straight_aux([], _, [], _).
calcular_large_straight_aux([Dado|Restantes], RestoStraight, [0|Patron], NDado) :-
	select(Dado, RestoStraight, NewRestoStraight), NewNDado is NDado + 1, 
	calcular_large_straight_aux(Restantes, NewRestoStraight, Patron, NewNDado).
calcular_large_straight_aux([Dado|Restantes], RestoStraight, [1|Patron], NDado) :-
	 \+ select(Dado, RestoStraight, _), 
	dado(NDado, Dado), NewNDado is NDado + 1, 
	calcular_large_straight_aux(Restantes, RestoStraight, Patron, NewNDado).
calcular_large_straight(Dados, [0, 0, 0, 0, 0]) :-
	large_straight(Straight), 
	sublist(Straight, Dados).
calcular_large_straight(Dados, Patron) :-
	large_straight(Straight), 
	calcular_large_straight_aux(Dados, Straight, Patron, 1).
calcular_patron(Dados, Patron, aces) :-
	calcular_patron_superior_aux(Dados, Patron, 1, 1).
calcular_patron(Dados, Patron, twos) :-
	calcular_patron_superior_aux(Dados, Patron, 2, 1).
calcular_patron(Dados, Patron, threes) :-
	calcular_patron_superior_aux(Dados, Patron, 3, 1).
calcular_patron(Dados, Patron, fours) :-
	calcular_patron_superior_aux(Dados, Patron, 4, 1).
calcular_patron(Dados, Patron, fives) :-
	calcular_patron_superior_aux(Dados, Patron, 5, 1).
calcular_patron(Dados, Patron, sixes) :-
	calcular_patron_superior_aux(Dados, Patron, 6, 1).
calcular_patron(Dados, Patron, three_of_a_kind) :-
	calcular_three_of_a_kind(Dados, Patron).
calcular_patron(Dados, Patron, four_of_a_kind) :-
	calcular_four_of_a_kind(Dados, Patron).
calcular_patron(Dados, Patron, full_house) :-
	calcular_patron_full_house(Dados, Patron).
calcular_patron(Dados, Patron, small_straight) :-
	calcular_small_straight(Dados, Patron).
calcular_patron(Dados, Patron, large_straight) :-
	calcular_large_straight(Dados, Patron).
calcular_patron(Dados, Patron, yahtzee) :-
	dado(N), 
	calcular_patron_superior_aux(Dados, Patron, N, 1).
calcular_patron(Dados, [0,0,0,0,0], chance).
