:- consult('../lib.pl').
:- dynamic count/3.

%%%%% Categorias Superiores

categoria_superior(aces, 1).
categoria_superior(twos, 2).
categoria_superior(threes, 3).
categoria_superior(fours, 4).
categoria_superior(fives, 5).
categoria_superior(sixes, 6).

%%%%% Three of a kind y Four of a kind
three_of_a_kind(Dados) :-
	count(Dados, N, Count), Count >= 3, !.
four_of_a_kind(Dados) :-
	count(Dados, N, Count), Count >= 4, !.

%%%%% Full House
full_house(Dados) :-
	select(X, Dados, DadosRestantes), 
	select(Y, DadosRestantes, DadosRestantesRestantes), X \= Y, 
	count(DadosRestantes, X, 2), 
	count(DadosRestantesRestantes, Y, 1).

%%%%% Small Straight
small_straight([1, 2, 3, 4]).
small_straight([2, 3, 4, 5]).
small_straight([3, 4, 5, 6]).
contains_small_straight(Dados) :-
	small_straight(Straight), 
	subset(Straight, Dados).
 
%%%%% Large Straight
large_straight([1, 2, 3, 4, 5]).
large_straight([2, 3, 4, 5, 6]).
contains_large_straight(Dados) :-
	large_straight(Straight), 
	subset(Straight, Dados).

%%%%% Yahtzee
yahtzee([X, X, X, X, X]).
