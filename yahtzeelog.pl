:- use_module(library(random)).
:- consult('common.pl').
:- consult('estrategias/humano.pl').
:- consult('estrategias/ia_det.pl').

% Setea el estado inicial del generador de números aleatorios
iniciar(X):-set_random(seed(X)).

% Tabla con las trece categorías
categorias([aces,twos,threes,fours,fives,sixes,three_of_a_kind,four_of_a_kind,full_house,small_straight,large_straight,yahtzee,chance]).

% Tablero inicial
inicial([s(aces,nil),s(twos,nil),s(threes,nil),s(fours,nil),s(fives,nil),s(sixes,nil),s(three_of_a_kind,nil),s(four_of_a_kind,nil),s(full_house,nil),s(small_straight,nil),s(large_straight,nil),s(yahtzee,nil),s(chance,nil)]).

% Lanza los dados, según el mapa que le pasamos en el segundo argumento
% Si en el mapa hay un 0, mantiene lo que había; de lo contrario, vuelve a lanzar ese dado
lanzamiento([],[],[]).
lanzamiento([X|T], [0|T1], [X|T2]):-
	lanzamiento(T, T1, T2).
lanzamiento([_|T], [1|T1], [X1|T2]):-
	tiro_dado(X1), 
	lanzamiento(T, T1, T2).

% Lanza un dado
tiro_dado(X):- random(1, 7, X).

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
cambio_dados(Dados, Tablero, humano, Patron) :- cambio_dados_h(Dados, Tablero, Patron).
cambio_dados(Dados, Tablero, ia_det, Patron) :- cambio_dados_det(Dados, Tablero, ia_det, Patron).


% Ver como jugarsela a la escalera (con 2 dados bajos (<3) o con 3 miembros de una escalera)
cambio_dados(Dados, Tablero, ia_det, Patron) :-
	count(Dados, N, Count), N >= 4, Count >= 2, 
	categoria_superior(Categoria, N), 
	categoria_disponible(Tablero, Categoria), !, 
	dados_distintos(Dados, N, Patron).
cambio_dados(Dados, Tablero, ia_det, Patron) :-
	count(Dados, N, Count), Count >= 3, 
	categoria_disponible(Tablero, yahtzee), !, 
	dados_distintos(Dados, N, Patron).
cambio_dados(Dados, Tablero, ia_det, Patron) :-
	count(Dados, N, Count), Count >= 3, N < 4, 
	categoria_disponible(Tablero, three_of_a_kind), !, 
	dados_distintos(Dados, N, Patron).
cambio_dados(Dados, Tablero, ia_det, Patron) :-
	count(Dados, N, Count), Count >= 3, N < 4, 
	categoria_disponible(Tablero, four_of_a_kind), !, 
	dados_distintos(Dados, N, Patron).

cambio_dados(_, _, ia_det, [1,1,1,1,1]).

%---------------------------------------------------------------------------
cambio_dados(Dados, Tablero, ia_prob, Patron) :-
	crear_queries(Dados, Queries, Tablero), 
	copiary_agregar_consultas('modelo_problog.pl', 'output.pl', Queries), % Se arma el output.pl con las queries dinamicas
	
	consultar_probabilidades(Valores), % Devuelve una lista Valores = [p(Dados, Patron, Categoria, probabilidad)].
	
	ponderar(Valores, ListaPonderada), % FALTA IMPLEMENTAR
	 
	seleccionar_mayor_patron(ListaPonderada, Patron).

calcular_puntaje_promedio([], [], Acc, Acc).
calcular_puntaje_promedio([Dado|Resto], [0|RestoPatron], Acc, Res) :-
	NewAcc is Acc + Dado, 
	calcular_puntaje_promedio(Resto, RestoPatron, NewAcc, Res).
calcular_puntaje_promedio([_|Resto], [1|RestoPatron], Acc, Res) :-
	NewAcc is Acc + 3.5, % En un dado de 6 caras el promedio es 3.5

	calcular_puntaje_promedio(Resto, RestoPatron, NewAcc, Res).

% puntaje(Dados, Patron, Categoria, Puntaje)
puntaje(_, _, aces, 5).
puntaje(_, _, twos, 10).
puntaje(_, _, threes, 15).
puntaje(_, _, fours, 20).
puntaje(_, _, fives, 25).
puntaje(_, _, sixes, 30).
puntaje(Dados, Patron, three_of_a_kind, Puntaje) :-
	calcular_puntaje_promedio(Dados, Patron, 0, Puntaje).
puntaje(Dados, Patron, four_of_a_kind, Puntaje) :-
	calcular_puntaje_promedio(Dados, Patron, 0, Puntaje).
puntaje(_, _, full_house, 25).
puntaje(_, _, small_straight, 30).
puntaje(_, _, large_straight, 40).
puntaje(_, _, yahtzee, 50).
puntaje(Dados, Patron, chance, Puntaje) :-
	calcular_puntaje_promedio(Dados, Patron, 0, Puntaje).

ponderar([],[]).
ponderar([p(Dados, Patron, Categoria, Probabilidad)|RestoValores], [(Patron, Ponderacion)|Resto]) :-
	puntaje(Dados, Patron, Categoria, Puntaje), Ponderacion is Puntaje*Probabilidad, 
	ponderar(RestoValores, Resto).

% Predicado para encontrar el patrón con la mayor ponderación, FUNCIONA
seleccionar_mayor_patron(ListaPonderada, PatronMaximo) :-
	seleccionar_mayor_patron_aux(ListaPonderada, 
		(_, 0), 
		(PatronMaximo, _)).

% Predicado auxiliar que recorre la lista y lleva registro del máximo actual
seleccionar_mayor_patron_aux([], MaxActual, MaxActual).
seleccionar_mayor_patron_aux([(Patron, Ponderacion)|Resto], (_, PonderacionMaxActual), Max) :-
	% Si la ponderación actual es mayor que la máxima actual, actualiza el máximo
Ponderacion > PonderacionMaxActual, 
	seleccionar_mayor_patron_aux(Resto, 
		(Patron, Ponderacion), Max).
seleccionar_mayor_patron_aux([(_, Ponderacion)|Resto], (PatronMaxActual, PonderacionMaxActual), Max) :-
	% Si la ponderación actual no es mayor, continúa con la misma máxima
Ponderacion =< PonderacionMaxActual, 
	seleccionar_mayor_patron_aux(Resto, 
		(PatronMaxActual, PonderacionMaxActual), Max).

mostrar_puntos_categoria(Dados, Categoria) :-
	eleccion_categoria(N, Categoria), 
	write(N), 
	write('. '), 
	write(Categoria), 
	puntaje(Dados, Categoria, Puntos), 
	write(':'), 
	writeln(Puntos).

mostrar_puntajes_aux(_, []).
mostrar_puntajes_aux(Dados, [s(CategoriaNombre, nil)|RestoCategorias]) :-
	mostrar_puntos_categoria(Dados, CategoriaNombre), 
	mostrar_puntajes_aux(Dados, RestoCategorias).
mostrar_puntajes_aux(Dados, [_|RestoCategorias]) :-
	mostrar_puntajes_aux(Dados, RestoCategorias).
mostrar_todos_puntajes(Dados, Tablero) :-
	writeln('Puntajes por categoria:'), 
	mostrar_puntajes_aux(Dados, Tablero).
eleccion_categoria(1, aces) :-
	!.
eleccion_categoria(2, twos) :-
	!.
eleccion_categoria(3, threes) :-
	!.
eleccion_categoria(4, fours) :-
	!.
eleccion_categoria(5, fives) :-
	!.
eleccion_categoria(6, sixes) :-
	!.
eleccion_categoria(7, three_of_a_kind) :-
	!.
eleccion_categoria(8, four_of_a_kind) :-
	!.
eleccion_categoria(9, full_house) :-
	!.
eleccion_categoria(10, small_straight) :-
	!.
eleccion_categoria(11, large_straight) :-
	!.
eleccion_categoria(12, yahtzee) :-
	!.
eleccion_categoria(13, chance) :-
	!.
leer_categoria(Categoria) :-
	writeln('Elija la categoria para la cual desea usar los dados:'), 
	readln([Input]), 
	eleccion_categoria(Input, Categoria), !, 
	write('Elejiste la categoria '), 
	writeln(Categoria).
leer_categoria(Categoria) :-
	writeln('Por favo ingrese un numero del 1 al 13 que corresponda a una categoria.'), 
	leer_categoria(Categoria).

puntaje_promedio(aces, 1.88).
puntaje_promedio(twos, 5.28).
puntaje_promedio(threes, 8.56).
puntaje_promedio(fours, 12.16).
puntaje_promedio(fives, 15.69).
puntaje_promedio(sixes, 19.81).
puntaje_promedio(three_of_a_kind, 21.66).
puntaje_promedio(four_of_a_kind, 13.09).
puntaje_promedio(full_house, 25).
puntaje_promedio(small_straight, 30).
puntaje_promedio(large_straight, 40).
puntaje_promedio(yahtzee, 50).
puntaje_promedio(chance, 22.01).

por_encima_promedio(Dados, Categoria) :-
	puntaje(Dados, Categoria, Puntaje), 
	puntaje_promedio(Categoria, Promedio), 
	Puntaje > Promedio.

encontrar_por_encima_del_promedio(Dados, Tablero, Categoria) :-
	findall(
		s(X, nil), 
		(member(
			s(X, nil), Tablero), 
			por_encima_promedio(Dados, X)), 
		[s(PrimeraCategoria, _)|RestoCategorias]), 
	maximo_puntaje_aux(Dados, RestoCategorias, PrimeraCategoria, Categoria).

maximo_puntaje_aux(_, [], Categoria, Categoria).
maximo_puntaje_aux(Dados, [s(CategoriaActual, nil)|CategoriasRestantes], CategoriaMaxima, Categoria) :-
	puntaje(Dados, CategoriaActual, Puntaje), 
	puntaje(Dados, CategoriaMaxima, PuntajeMaximo), Puntaje >= PuntajeMaximo, 
	maximo_puntaje_aux(Dados, CategoriasRestantes, CategoriaActual, Categoria).
maximo_puntaje_aux(Dados, [s(CategoriaActual, nil)|CategoriasRestantes], CategoriaMaxima, Categoria) :-
	puntaje(Dados, CategoriaActual, Puntaje), 
	puntaje(Dados, CategoriaMaxima, PuntajeMaximo), Puntaje < PuntajeMaximo, 
	maximo_puntaje_aux(Dados, CategoriasRestantes, CategoriaMaxima, Categoria).
maximo_puntaje(Dados, Tablero, Categoria) :-
	findall(
		s(X, nil), 
		member(
			s(X, nil), Tablero), 
		[s(PrimeraCategoria, _)|RestoCategorias]), 
	maximo_puntaje_aux(Dados, RestoCategorias, PrimeraCategoria, Categoria).

elegir_categoria(Dados, Tablero, Categoria) :-
	encontrar_por_encima_del_promedio(Dados, Tablero, Categoria), !.
elegir_categoria(Dados, Tablero, Categoria) :-
	maximo_puntaje(Dados, Tablero, Categoria), !.

eleccion_slot(Dados, Tablero, humano, Categoria) :-
	!, 
	writeln('Este es el tablero actual:'), 
	writeln(Tablero), 
	writeln('Estos son tus dados:'), 
	writeln(Dados), 
	mostrar_todos_puntajes(Dados, Tablero), 
	leer_categoria(Categoria), !.
eleccion_slot(Dados, Tablero, ia_det, Categoria) :-
	categoria_superior(Categoria, N), N >= 4, 
	categoria_disponible(Tablero, Categoria), 
	count(Dados, N, Count), Count >= 4, !.
eleccion_slot(Dados, Tablero, ia_det, yahtzee) :-
	categoria_disponible(Tablero, yahtzee), 
	puntaje(Dados, yahtzee, 50), !.
eleccion_slot(Dados, Tablero, ia_det, full_house) :-
	categoria_disponible(Tablero, full_house), 
	puntaje(Dados, full_house, 25), !.
eleccion_slot(Dados, Tablero, ia_det, Categoria) :-
	categoria_superior(Categoria, N), 
	categoria_disponible(Tablero, Categoria), 
	count(Dados, N, Count), Count >= 2, !.
eleccion_slot(Dados, Tablero, ia_det, Categoria) :-
	categoria_superior(Categoria, N), 
	categoria_disponible(Tablero, Categoria), N < 4, !.

% Elijo la categoria disponible con mayor puntaje
eleccion_slot(Dados, Tablero, ia_det, Categoria) :-
	elegir_categoria(Dados, Tablero, Categoria).

eleccion_slot(Dados, Tablero, ia_prob, Categoria) :-
	elegir_categoria(Dados, Tablero, Categoria).
