:- consult('categorias.pl').

% puntaje(+Dados, +Cat, -Puntos) devuelve en Puntos el puntaje obtenido al asignar la categoría Cat a los dados Dados
puntaje(Dados, Superior, Puntos) :-
	categoria_superior(Superior, N), !, 
	puntaje_superior(Dados, N, Puntos).
puntaje(Dados, three_of_a_kind, Puntos) :-
	three_of_a_kind(Dados), !, 
	sumlist(Dados, Puntos).
puntaje(Dados, four_of_a_kind, Puntos) :-
	four_of_a_kind(Dados), !, 
	sumlist(Dados, Puntos).
puntaje(Dados, full_house, 25) :-
	full_house(Dados), !.
puntaje(Dados, small_straight, 30) :-
	contains_small_straight(Dados), !.
puntaje(Dados, large_straight, 40) :-
	contains_large_straight(Dados), !.
puntaje(Dados, yahtzee, 50) :-
	yahtzee(Dados), !.
puntaje(Dados, chance, Puntos) :-
	sumlist(Dados, Puntos), !.
puntaje(_, _, 0).

%%%%% Utility functions

puntaje_superior_aux(_, _, Acc, Acc).
puntaje_superior_aux([N|Restante], N, Acc, Puntos) :-
	NewAcc is Acc + N, 
	puntaje_superior_aux(Restante, N, NewAcc, Puntos).
puntaje_superior_aux([Dado|Restante], N, Acc, Puntos) :-
	Dado \= N, 
	puntaje_superior_aux(Restante, N, Acc, Puntos).
puntaje_superior(Dados, N, Puntos) :-
	puntaje_superior_aux(Dados, N, 0, Puntos).
