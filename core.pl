:- consult('lib.pl').

categorias([aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]).

categoria_superior(aces, 1).
categoria_superior(twos, 2).
categoria_superior(threes, 3).
categoria_superior(fours, 4).
categoria_superior(fives, 5).
categoria_superior(sixes, 6).

three_of_a_kind(Dados) :- count(Dados, N, 3).
four_of_a_kind(Dados) :- count(Dados, N, 4).

full_house(Dados) :-
    select(X, Dados, DadosRestantes),
    select(Y, DadosRestantes, DadosRestantesRestantes),
    X \= Y,
    count(DadosRestantes, X, 2),
    count(DadosRestantesRestantes, Y, 1).

small_straight([1, 2, 3, 4]).
small_straight([2, 3, 4, 5]).
small_straight([3, 4, 5, 6]).

large_straight([1, 2, 3, 4, 5]).
large_straight([2, 3, 4, 5, 6]).

yahtzee([X, X, X, X, X]).

chance(_).

matches(Cat, _) :- categoria_superior(Cat, _).
matches(three_of_a_kind, Dados) :- three_of_a_kind(Dados).
matches(four_of_a_kind, Dados) :- four_of_a_kind(Dados).
matches(full_house, Dados) :- full_house(Dados).
matches(small_straight, Dados) :- small_straight(Dados).
matches(large_straight, Dados) :- large_straight(Dados).
matches(yahtzee, Dados) :- yahtzee(Dados).
matches(chance, _).
 