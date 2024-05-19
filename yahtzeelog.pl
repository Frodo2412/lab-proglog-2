:- use_module(library(random)).

% Setea el estado inicial del generador de números aleatorios
iniciar(X):- set_random(seed(X)).

% Tabla con las trece categorías
categorias([aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]).

% Tablero inicial
inicial([s(aces,nil),s(twos,nil),s(threes,nil),s(fours,nil),s(fives,nil),s(sixes,nil),s(three_of_a_kind,nil),s(four_of_a_kind,nil),s(full_house,nil),s(small_straight,nil),s(large_straight,nil),s(yahtzee,nil),s(chance,nil)]).

% Lanza los dados, según el mapa que le pasamos en el segundo argumento
% Si en el mapa hay un 0, mantiene lo que había; de lo contrario, vuelve a lanzar ese dado
lanzamiento([],[],[]).
lanzamiento([X|T],[0|T1],[X|T2]):-
    lanzamiento(T,T1,T2).
lanzamiento([_|T],[1|T1],[X1|T2]):-
    tiro_dado(X1),
    lanzamiento(T,T1,T2).

% Lanza un dado
tiro_dado(X):-
    random(1,7,X).

% Cuenta cuántas veces aparece un elemento en una lista
count([], _, 0).
count([X|T], X, Y) :- count(T,X,Z), Y is 1+Z.
count([X1|T], X, Z) :- X1\=X, count(T,X,Z).

% calu

% calcular_categoria_superior(+Dados, +Categoria, -Puntaje) calcula el puntaje obtenido al asignar la categoría superior Categoria a los dados Dados
calcular_categoria_superior(Dados, Categoria, Puntaje) :-
    count(Dados, Categoria, Ocurrencias),
    Puntaje is Categoria * Ocurrencias.

calcular_three_of_a_kind(Dados, Puntaje) :-
    member(X, Dados),
    count(Dados, X, Ocurrencias),
    Ocurrencias >= 3, !,
    sumlist(Dados, Puntaje).
calcular_three_of_a_kind(_, 0).

calcular_four_of_a_kind(Dados, Puntaje) :-
    member(X, Dados),
    count(Dados, X, Ocurrencias),
    Ocurrencias >= 4, !,
    sumlist(Dados, Puntaje).
calcular_four_of_a_kind(_, 0).

calcular_full_house(Dados, Puntaje) :-
    select(X, Dados, DadosRestantes),
    select(Y, DadosRestantes, DadosRestantesRestantes),
    X \= Y, !,
    count(DadosRestantes, X, Ocurrencias),
    Ocurrencias = 2, !,
    count(DadosRestantesRestantes, Y, Ocurrencias2),
    Ocurrencias2 = 1, !,
    Puntaje is 25.

is_straight(_, _, 0).
is_straight(X, Dados, Largo) :-
    select(X, Dados, Restantes),
    Next is X + 1,
    NextLargo is Largo - 1,
    is_straight(Next, Restantes, NextLargo).

calcular_small_straight(Dados, Puntaje) :-
    sort(Dados, DadosOrdenados),
    member(X, DadosOrdenados),
    Next is X + 1,
    is_straight(Next, DadosOrdenados, 3), !,
    Puntaje is 30.
calcular_small_straight(_, 0).

large_straight([1,2,3,4,5], 40) :- !.
large_straight([2,3,4,5,6], 40) :- !.
large_straight(_, 0).

calcular_large_straight(Dados, Puntaje) :- 
    sort(Dados, DadosOrdenados), large_straight(Dados, Puntaje).

calcular_yahtzee([X,X,X,X,X], 50) :- !.
calcular_yahtzee(_, 0).

calcular_chance(Dados, Puntaje) :- sumlist(Dados, Puntaje).

% puntaje(+Dados, +Cat, -Puntos) devuelve en Puntos el puntaje obtenido al asignar la categoría Cat a los dados Dados
puntaje(Dados, aces, Puntos) :- calcular_categoria_superior(Dados, 1, Puntos).
puntaje(Dados, twos, Puntos) :- calcular_categoria_superior(Dados, 2, Puntos).
puntaje(Dados, threes, Puntos) :- calcular_categoria_superior(Dados, 3, Puntos).
puntaje(Dados, fours, Puntos) :- calcular_categoria_superior(Dados, 4, Puntos).
puntaje(Dados, fives, Puntos) :- calcular_categoria_superior(Dados, 5, Puntos).
puntaje(Dados, sixes, Puntos) :- calcular_categoria_superior(Dados, 6, Puntos).
puntaje(Dados, three_of_a_kind, Puntos) :- calcular_three_of_a_kind(Dados, Puntos).
puntaje(Dados, four_of_a_kind, Puntos) :- calcular_four_of_a_kind(Dados, Puntos).
puntaje(Dados, full_house, Puntos) :- calcular_full_house(Dados, Puntos).
puntaje(Dados, small_straight, Puntos) :- calcular_small_straight(Dados, Puntos).
puntaje(Dados, large_straight, Puntos) :- calcular_large_straight(Dados, Puntos).
puntaje(Dados, yahtzee, Puntos) :- calcular_yahtzee(Dados, Puntos).
puntaje(Dados, chance, Puntos) :- calcular_chance(Dados, Puntos).


% Game Loop
% 1. Tirar dados
% 2. Elegir categoría
% 3. Calcular puntaje
% 4. Actualizar tablero
