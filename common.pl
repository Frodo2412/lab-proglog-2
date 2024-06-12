% Cuenta cuántas veces aparece un elemento en una lista

small_straight([1,2,3,4]).
small_straight([2,3,4,5]).
small_straight([3,4,5,6]).

has_small_straight(Dados) :-
    small_straight(SmallStraight),
    subset(SmallStraight, Dados).

large_straight([1,2,3,4,5]).
large_straight([2,3,4,5,6]).

has_large_straight(Dados) :-
    large_straight(LargeStraight),
    subset(LargeStraight, Dados).

% calcular_categoria_superior(+Dados, +Categoria, -Puntaje) calcula el puntaje obtenido al asignar la categoría superior Categoria a los dados Dados
calcular_categoria_superior(Dados, Categoria, Puntaje) :-
	count(Dados, Categoria, Ocurrencias), Puntaje is Categoria*Ocurrencias.
calcular_three_of_a_kind(Dados, Puntaje) :-
	member(X, Dados), 
	count(Dados, X, Ocurrencias), Ocurrencias >= 3, !, 
	sumlist(Dados, Puntaje).

calcular_three_of_a_kind(_, 0).
calcular_four_of_a_kind(Dados, Puntaje) :-
	member(X, Dados), 
	count(Dados, X, Ocurrencias), Ocurrencias >= 4, !, 
	sumlist(Dados, Puntaje).
calcular_four_of_a_kind(_, 0).

calcular_full_house(Dados, Puntaje) :-
	select(X, Dados, DadosRestantes), 
	select(Y, DadosRestantes, DadosRestantesRestantes), X \= Y, 
	count(DadosRestantes, X, Ocurrencias), Ocurrencias = 2, 
	count(DadosRestantesRestantes, Y, Ocurrencias2), Ocurrencias2 = 1, !, Puntaje is 25.
calcular_full_house(_, 0).

is_straight(_, _, 0).
is_straight(X, Dados, Largo) :-
	select(X, Dados, Restantes), Next is X + 1, NextLargo is Largo - 1, 
	is_straight(Next, Restantes, NextLargo).

calcular_small_straight(Dados, Puntaje) :-
	sort(Dados, DadosOrdenados), 
	member(X, DadosOrdenados), Next is X + 1, 
	is_straight(Next, DadosOrdenados, 3), !, Puntaje is 30.
calcular_small_straight(_, 0).
large_straight([1, 2, 3, 4, 5], 40) :-
	!.
large_straight([2, 3, 4, 5, 6], 40) :-
	!.
large_straight(_, 0).
calcular_large_straight(Dados, Puntaje) :-
	sort(Dados, DadosOrdenados), 
	large_straight(DadosOrdenados, Puntaje).
calcular_yahtzee([X, X, X, X, X], 50) :-
	!.
calcular_yahtzee(_, 0).
calcular_chance(Dados, Puntaje) :-
	sumlist(Dados, Puntaje).

categoria_superior(aces, 1).
categoria_superior(twos, 2).
categoria_superior(threes, 3).
categoria_superior(fours, 4).
categoria_superior(fives, 5).
categoria_superior(sixes, 6).

% puntaje(+Dados, +Cat, -Puntos) devuelve en Puntos el puntaje obtenido al asignar la categoría Cat a los dados Dados

