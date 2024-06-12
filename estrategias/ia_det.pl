:- consult('../lib.pl').
:- consult('../core/categorias.pl').
:- consult('../core/tablero.pl').
:- dynamic yahtzee/1, has_large_straight/1, categoria_disponible/2, full_house/1, four_of_a_kind/1, three_of_a_kind/1, small_straight/1, count/3.

% cambios_dados_det(+Tablero, +Dados, -Patron)
cambio_dados_det(Tablero, Dados, [0, 0, 0, 0, 0], _) :-
	yahtzee(Dados), 
	categoria_disponible(Tablero, yahtzee).
cambio_dados_det(Tablero, Dados, [0, 0, 0, 0, 0], _) :-
	has_large_straight(Dados), 
	categoria_disponible(Tablero, large_straight).
cambio_dados_det(Tablero, Dados, [0, 0, 0, 0, 0], _) :-
	outside_straight(Dados), 
	categoria_disponible(Tablero, small_straight).
cambio_dados_det(Tablero, Dados, Patron, 1) :-
    cambio_dados_1(Tablero, Dados, Patron).

% cambio_dados_1(Tablero, Dados, Patron)
cambio_dados_1(_, Dados, Patron) :-
	inside_straight(Dados), 
	(
		pair(Dados, X)
		 ->
				(
			x =< 2 ->
				hold_straight(Dados, Patron);
			hold(X, Dados, Patron));
		hold_straight(Dados, Patron)).
cambio_dados_1(_, Dados, Patron) :-
	full_house(Dados), 
	hold_n_of_a_kind(3,Dados, Patron).
cambio_dados_1(_, Dados, Patron) :-
	three_of_a_kind(Dados), 
	hold_n_of_a_kind(3, Dados, Patron).
cambio_dados_1(_, Dados, Patron) :-
	four_of_a_kind(Dados), 
	hold_n_of_a_kind(4, Dados, Patron).
cambio_dados_1(_, Dados, Patron) :-
	pair(Dados, X), 
	pair(Dados, Y), X \= Y, 
	(
		X < Y ->
			hold(Y, Dados, Patron);
		hold(X, Dados, Patron)).
cambio_dados_1(_, Dados, Patron) :-
	 \+ small_straight(Dados), 
	pair(Dados, X), 
	(
		X >= 2 ->
			hold(X, Dados, Patron);
		(
			subset([3, 4, 5], Dados)
			 ->
					hold_set([3, 4, 5], Dados, Patron);
			(
				member(5, Dados)
				 ->
						hold(5, Dados, Patron);
				(
					member(4, Dados)
					 ->
							hold(4, Dados, Patron);
					hold(6, Dados, Patron))))).
cambio_dados_1(_, Dados, Patron) :-
        \+ small_straight(Dados), 
        \+ pair(Dados, _), 
        hold(5, Dados, Patron).

%%%%% Utility Functions

% pair(+Dados, -X)
pair(Dados, X) :-count(Dados, X, 2), !.

% Inside and outside straights
outside_straight(Dados) :-
	subset([2, 3, 4, 5], Dados).
inside_straight(Dados) :-
	subset([1, 2, 3, 4], Dados).
inside_straight(Dados) :-
	subset([3, 4, 5, 6], Dados).

% Hold a number
hold(_, [], []).
hold(N, [N|Restantes], [0|Patron]) :-
	hold(N, Restantes, Patron).
hold(N, [Dado|Restantes], [Dado|Patron]) :-
	Dado \= N, 
	hold(N, Restantes, Patron).

% Hold straight
hold_straight(Dados, Patron) :-
	small_straight(SmallStraight), 
	hold_set(SmallStraight, Dados, Patron).

hold_set(_, [], []).
hold_set(Straight, [Dado|Restantes], [0|Patron]) :-
	select(Dado, Straight, StraightRestantes), 
	hold_set(StraightRestantes, Restantes, Patron).

% Hold three of a kind
hold_n_of_a_kind(M, Dados, Patron) :-
	count(Dados, N, M), !, 
	hold(N, Dados, Patron).
