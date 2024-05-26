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
    X \= Y,
    count(DadosRestantes, X, Ocurrencias),
    Ocurrencias = 2,
    count(DadosRestantesRestantes, Y, Ocurrencias2),
    Ocurrencias2 = 1, !,
    Puntaje is 25.
calcular_full_house(_, 0).

is_straight(_, _, 0).
is_straight(X, Dados, Largo) :-
    select(X, Dados, Restantes),
    Next is X + 1,
    NextLargo is Largo - 1,
    is_straight(Next, Restantes, NextLargo).

% 1. Numero duplicado [1,2,3,4,4]
% 2. Bache en la escalera [1,3,4,5,6] [1,2,3,4,6]
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
    sort(Dados, DadosOrdenados), large_straight(DadosOrdenados, Puntaje).

calcular_yahtzee([X,X,X,X,X], 50) :- !.
calcular_yahtzee(_, 0).

calcular_chance(Dados, Puntaje) :- sumlist(Dados, Puntaje).

categoria_superior(aces, 1).
categoria_superior(twos, 2).
categoria_superior(threes, 3).
categoria_superior(fours, 4).
categoria_superior(fives, 5).
categoria_superior(sixes, 6).

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

% puntaje_tablero(+Tablero, -Puntaje) Dado un tablero que tiene todos los slots completos devuelve el total de puntos

categoria_superior(aces).
categoria_superior(twos).
categoria_superior(threes).
categoria_superior(fours).
categoria_superior(fives).
categoria_superior(sixes).

calcular_categorias([CategoriaActual|RestoCategorias], PuntajeSuperior, PuntajeInferior, Puntaje) :-
    arg(1, CategoriaActual, NombreCategoria),
    arg(2, CategoriaActual, PuntajeCategoria),
    categoria_superior(NombreCategoria), !,
    NuevoPuntaje is PuntajeSuperior + PuntajeCategoria,
    calcular_categorias(RestoCategorias, NuevoPuntaje, PuntajeInferior, Puntaje).
calcular_categorias([CategoriaActual|RestoCategorias], PuntajeSuperior, PuntajeInferior, Puntaje) :-
    arg(2, CategoriaActual, PuntajeCategoria),
    NuevoPuntaje is PuntajeInferior + PuntajeCategoria,
    calcular_categorias(RestoCategorias, PuntajeSuperior, NuevoPuntaje, Puntaje).
calcular_categorias([],  PuntajeSuperior, PuntajeInferior, Puntaje) :-
    PuntajeSuperior >= 63, !,
    Puntaje is PuntajeSuperior + PuntajeInferior + 35.
calcular_categorias([],  PuntajeSuperior, PuntajeInferior, Puntaje) :-
    Puntaje is PuntajeSuperior + PuntajeInferior.

puntaje_tablero(Tablero, Puntaje) :- calcular_categorias(Tablero, 0, 0, Puntaje).

% ajustar_tablero(+Tablero, +Categoria, +Puntaje, -TableroSalida)
ajustar_tablero([CategoriaActual | RestoCategorias], Categoria, Puntaje, [CategoriaActual|RestoTableroSalida]) :- 
    ajustar_tablero(RestoCategorias, Categoria, Puntaje, RestoTableroSalida).
ajustar_tablero([s(Categoria, _)|RestoCategorias], Categoria, Puntaje, [s(Categoria, Puntaje) | RestoCategorias]).

leer_lista(5, _).
leer_lista(N, [Input|T]) :-
    writeln('Indica si quieres volver a tirar el dado (1 si, 0 si):'),
    readln([Input]),
    member(Input, [1,0]), !,
    M is N + 1,
    leer_lista(M,  T).
leer_lista(N, [Input|T]) :-
    writeln('Por favor ingresa 0 o 1.'),
    leer_lista(N,  [Input|T]).

categoria_disponible(Tablero, Categoria) :-
    member(s(Categoria, nil), Tablero).

% es verdadero si Patron es una lista de 5 elementos que indica si se debe volver a tirar el dado correspondiente
dados_distintos([], _, []).
dados_distintos([Dado|Restantes], N, [0|RestoPatron]) :-
    Dado = N, !,
    dados_distintos(Restantes, N, RestoPatron).
dados_distintos([_|Restantes], N, [1|RestoPatron]) :-
    dados_distintos(Restantes, N, RestoPatron).

% X = 6, [2,4,6,3,1] => [0,0,1,0,0]
recorrer_escalera(_, [], []).
recorrer_escalera(X, [Dado|Restantes], [1|PatronRestante]) :-
    Dado = X, !,
    recorrer_escalera(X, Restantes, PatronRestante).
recorrer_escalera(X, [_|Restantes], [0|PatronRestante]) :-
    recorrer_escalera(X, Restantes, PatronRestante).

patron_escalera(Dados, Patron) :-
    sort(Dados, [1, 2, _, _, 6]), !,
    recorrer_escalera(6, Dados, Patron).

patron_escalera(Dados, Patron) :-
    sort(Dados, [1, 3, _, _, 6]), !,
    recorrer_escalera(1, Dados, Patron).

% cambio_dados
cambio_dados(Dados, Tablero, humano, Patron) :-
    writeln('Este es el tablero actual:'),
    writeln(Tablero),
    writeln('Estos son tus dados:'),
    writeln(Dados),
    leer_lista(0, Patron).

cambio_dados(Dados, Tablero, ia_det, [0,0,0,0,0]) :-
    is_straight(1, Dados, 5),
    categoria_disponible(Tablero, large_straight), !.

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    sort(Dados, [Dado | Restantes]),
    is_straight(Dado, Restantes, 4),
    categoria_disponible(Tablero, small_straight), !,
    patron_escalera(Dados, Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    count(Dados, N, Count),
    Count >= 3,
    categoria_superior(Categoria, N),
    categoria_disponible(Tablero, Categoria), !,
    dados_distintos(Dados, N, Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    count(Dados, N, Count),
    Count >= 3,
    categoria_disponible(Tablero, yahtzee), !,
    dados_distintos(Dados, N, Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    count(Dados, N, Count),
    Count >= 3,
    N < 4,
    categoria_disponible(Tablero, three_of_a_kind), !,
    dados_distintos(Dados, N, Patron).

cambio_dados(Dados, Tablero, ia_det, Patron) :-
    count(Dados, N, Count),
    Count >= 3,
    N < 4,
    categoria_disponible(Tablero, four_of_a_kind), !,
    dados_distintos(Dados, N, Patron).

cambio_dados(Dados, Tablero, ia_det, [0,0,0,0,0]) :-
    count(Dados, N, 2),
    count(Dados, M, 3),
    N \= M,
    categoria_disponible(Tablero, full_house), !.

cambio_dados(_, _, ia_det, [1,1,1,1,1]).

% cambio_dados(Dados, Tablero, ia_prob, Patron) :-
%     % Aca hay que usar problog

mostrar_puntos_categoria(Dados, Categoria) :-
    eleccion_categoria(N, Categoria),
    write(N), write('. '),
    write(Categoria),
    puntaje(Dados, Categoria, Puntos),
    write(':'),
    writeln(Puntos).

mostrar_puntajes_aux(_, []).
mostrar_puntajes_aux(Dados, [s(CategoriaNombre, nil) | RestoCategorias]) :-
    mostrar_puntos_categoria(Dados, CategoriaNombre),
    mostrar_puntajes_aux(Dados, RestoCategorias).
mostrar_puntajes_aux(Dados, [_|RestoCategorias]) :-
    mostrar_puntajes_aux(Dados, RestoCategorias).

mostrar_todos_puntajes(Dados, Tablero) :-
    writeln('Puntajes por categoria:'),
    mostrar_puntajes_aux(Dados, Tablero).

eleccion_categoria(1, aces) :- !.
eleccion_categoria(2, twos) :- !.
eleccion_categoria(3, threes) :- !.
eleccion_categoria(4, fours) :- !.
eleccion_categoria(5, fives) :- !.
eleccion_categoria(6, sixes) :- !.
eleccion_categoria(7, three_of_a_kind) :- !.
eleccion_categoria(8, four_of_a_kind) :- !.
eleccion_categoria(9, full_house) :- !.
eleccion_categoria(10, small_straight) :- !.
eleccion_categoria(11, large_straight) :- !.
eleccion_categoria(12, yahtzee) :- !.
eleccion_categoria(13, chance) :- !.

leer_categoria(Categoria) :-
    writeln('Elija la categoria para la cual desea usar los dados:'),
    readln([Input]),
    eleccion_categoria(Input, Categoria), !,
    write('Elejiste la categoria '), writeln(Categoria).
leer_categoria(Categoria) :-
    writeln('Por favo ingrese un numero del 1 al 13 que corresponda a una categoria.'),
    leer_categoria(Categoria).


maximo_puntaje_aux(_, [], Categoria, Categoria).
maximo_puntaje_aux(Dados, [s(CategoriaActual, nil)| CategoriasRestantes], CategoriaMaxima, Categoria) :-
    puntaje(Dados, CategoriaActual, Puntaje),
    puntaje(Dados, CategoriaMaxima, PuntajeMaximo),
    Puntaje >= PuntajeMaximo,
    maximo_puntaje_aux(Dados, CategoriasRestantes, CategoriaActual, Categoria).
maximo_puntaje_aux(Dados, [s(CategoriaActual, nil)| CategoriasRestantes], CategoriaMaxima, Categoria) :-
    puntaje(Dados, CategoriaActual, Puntaje),
    puntaje(Dados, CategoriaMaxima, PuntajeMaximo),
    Puntaje < PuntajeMaximo,
    maximo_puntaje_aux(Dados, CategoriasRestantes, CategoriaMaxima, Categoria).

maximo_puntaje(Dados, Tablero, Categoria) :-
    findall(s(X, nil), member(s(X, nil), Tablero), [s(PrimeraCategoria,_)|RestoCategorias]),
maximo_puntaje_aux(Dados, RestoCategorias, PrimeraCategoria, Categoria).
    

eleccion_slot(Dados, Tablero, humano, Categoria) :- !,
    writeln('Este es el tablero actual:'),
    writeln(Tablero),
    writeln('Estos son tus dados:'),
    writeln(Dados),
    mostrar_todos_puntajes(Dados, Tablero),
    leer_categoria(Categoria), !.

eleccion_slot(Dados, Tablero, ia_det, Categoria) :-
    categoria_superior(Categoria, N),
    N >=3, categoria_disponible(Tablero, Categoria), 
    count(Dados, N, Count), Count >= 4, !.

eleccion_slot(Dados, Tablero, ia_det, yahtzee) :-
    categoria_disponible(Tablero, yahtzee),
    count(Dados, N, Count), Count >= 5, !.

% Elijo la categoria disponible con mayor puntaje
eleccion_slot(Dados, Tablero, ia_det, Categoria) :- 
    maximo_puntaje(Dados, Tablero, Categoria).

% Game Loop
% 1. Preguntar dados a re rollear
% 2. Tirar Dados
% 2. Elegir categoría a usar
% 3. Actualizar Tablero en el ambiente
% 4. Repetir


% Ronda:
% lanzamiento inicial
% Hasta 3 veces
% cambio_dados(Dados, TableroActual, _, patron)
% lanzamiento(???)
% eleccion_slot(Dados, TableroActual, _, Categoria)
% puntaje(Dados, Categoria, Puntaje)
% ajustar_tablero(TableroActual, Categoria, Puntaje, NuevoTablero)
% nb_setarg(TableroActual, NuevoTablero)
