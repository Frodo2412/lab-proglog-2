:- use_module(library(random)).

% Setea el estado inicial del generador de números aleatorios
iniciar(X):-
	set_random(
		seed(X)).

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
tiro_dado(X):-
	random(1, 7, X).

% Cuenta cuántas veces aparece un elemento en una lista
% count(+Lista, +Elemento, -Cantidad) -> count(+Lista, ?Elemento, ?Cantidad)
count_aux(Lista, N, Acc, Acc) :-
	 \+ select(N, Lista, _).
count_aux(Lista, N, Acc, Count) :-
	select(N, Lista, Resto), NewAcc is Acc + 1, 
	count_aux(Resto, N, NewAcc, Count).
count(Lista, N, Count) :-
	count_aux(Lista, N, 0, Count).


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

% 1. Numero duplicado [1,2,3,4,4]
% 2. Bache en la escalera [1,3,4,5,6] [1,2,3,4,6]
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
puntaje(Dados, aces, Puntos) :-
	calcular_categoria_superior(Dados, 1, Puntos).
puntaje(Dados, twos, Puntos) :-
	calcular_categoria_superior(Dados, 2, Puntos).
puntaje(Dados, threes, Puntos) :-
	calcular_categoria_superior(Dados, 3, Puntos).
puntaje(Dados, fours, Puntos) :-
	calcular_categoria_superior(Dados, 4, Puntos).
puntaje(Dados, fives, Puntos) :-
	calcular_categoria_superior(Dados, 5, Puntos).
puntaje(Dados, sixes, Puntos) :-
	calcular_categoria_superior(Dados, 6, Puntos).
puntaje(Dados, three_of_a_kind, Puntos) :-
	calcular_three_of_a_kind(Dados, Puntos).
puntaje(Dados, four_of_a_kind, Puntos) :-
	calcular_four_of_a_kind(Dados, Puntos).
puntaje(Dados, full_house, Puntos) :-
	calcular_full_house(Dados, Puntos).
puntaje(Dados, small_straight, Puntos) :-
	calcular_small_straight(Dados, Puntos).
puntaje(Dados, large_straight, Puntos) :-
	calcular_large_straight(Dados, Puntos).
puntaje(Dados, yahtzee, Puntos) :-
	calcular_yahtzee(Dados, Puntos).
puntaje(Dados, chance, Puntos) :-
	calcular_chance(Dados, Puntos).

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
	categoria_superior(NombreCategoria), !, NuevoPuntaje is PuntajeSuperior + PuntajeCategoria, 
	calcular_categorias(RestoCategorias, NuevoPuntaje, PuntajeInferior, Puntaje).
calcular_categorias([CategoriaActual|RestoCategorias], PuntajeSuperior, PuntajeInferior, Puntaje) :-
	arg(2, CategoriaActual, PuntajeCategoria), NuevoPuntaje is PuntajeInferior + PuntajeCategoria, 
	calcular_categorias(RestoCategorias, PuntajeSuperior, NuevoPuntaje, Puntaje).
calcular_categorias([], PuntajeSuperior, PuntajeInferior, Puntaje) :-
	PuntajeSuperior >= 63, !, Puntaje is PuntajeSuperior + PuntajeInferior + 35.
calcular_categorias([], PuntajeSuperior, PuntajeInferior, Puntaje) :-
	Puntaje is PuntajeSuperior + PuntajeInferior.
puntaje_tablero(Tablero, Puntaje) :-
	calcular_categorias(Tablero, 0, 0, Puntaje).

% ajustar_tablero(+Tablero, +Categoria, +Puntaje, -TableroSalida)
ajustar_tablero([CategoriaActual|RestoCategorias], Categoria, Puntaje, [CategoriaActual|RestoTableroSalida]) :-
	ajustar_tablero(RestoCategorias, Categoria, Puntaje, RestoTableroSalida).
ajustar_tablero([s(Categoria, _)|RestoCategorias], Categoria, Puntaje, [s(Categoria, Puntaje) | RestoCategorias]).

leer_lista(5, _).
leer_lista(N, [Input|T]) :-
	writeln('Indica si quieres volver a tirar el dado (1 si, 0 si):'), 
	readln([Input]), 
	member(Input, [1, 0]), !, M is N + 1, 
	leer_lista(M, T).
leer_lista(N, [Input|T]) :-
	writeln('Por favor ingresa 0 o 1.'), 
	leer_lista(N, [Input|T]).
categoria_disponible(Tablero, Categoria) :-
	member(
		s(Categoria, nil), Tablero).

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
cambio_dados(Dados, Tablero, ia_det, [0, 0, 0, 0, 0]) :-
	count(Dados, N, 2), 
	count(Dados, M, 3), N \= M, 
	categoria_disponible(Tablero, full_house), !.
% Ver como tirar solo 2 dados
cambio_dados(Dados, Tablero, ia_det, Patron) :-
	count(Dados, N, 2), N =< 3, 
	categoria_disponible(Tablero, full_house), !, 
	dados_distintos(Dados, N, Patron).
% Ver como tirar un solo dado
cambio_dados(Dados, Tablero, ia_det, Patron) :-
	count(Dados, N, 3), N =< 3, 
	categoria_disponible(Tablero, full_house), !, 
	dados_distintos(Dados, N, Patron).
cambio_dados(Dados, Tablero, ia_det, [0, 0, 0, 0, 0]) :-
	is_straight(1, Dados, 5), 
	categoria_disponible(Tablero, large_straight), !.
cambio_dados(Dados, Tablero, ia_det, Patron) :-
	sort(Dados, [Dado|Restantes]), 
	is_straight(Dado, Restantes, 4), 
	categoria_disponible(Tablero, small_straight), !, 
	patron_escalera(Dados, Patron).

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

categorias_disponibles(Tablero, Categorias) :-
	findall(
		Categoria, 
		(member(s(Categoria, nil), Tablero)), 
		Categorias).

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
crear_queries(Dados, Queries, Tablero) :-
	categorias_disponibles(Tablero, Categorias),
	build_queries_for_categories(Dados, Categorias, Queries).

% Ver de filtrar categorias que no esten disponibles
build_queries_for_categories(_, [], []).
build_queries_for_categories(Dados, [chance|RestoCategorias], Queries) :-
	!, 
	build_queries_for_categories(Dados, RestoCategorias, Queries).
build_queries_for_categories(Dados, [Categoria|RestoCategorias], [Querie|RestoQueries]) :-
	atomic_list_concat(Dados, ',', DadosStringConComas), 
	atom_concat('[', DadosStringConComas, DadosStringConCorcheteIzquierdo), 
	atom_concat(DadosStringConCorcheteIzquierdo, ']', DadosString), 
	atom_string(Categoria, CategoriaString), 
	atom_concat(DadosString, ', Patron, ', Params1), 
	atom_concat(Params1, CategoriaString, Params2), 
	atom_concat('query(calcular_patron(', Params2, Params3), 
	atom_concat(Params3, ')).', Querie), 
	build_queries_for_categories(Dados, RestoCategorias, RestoQueries).
consultar_probabilidades(ListaValores):-
	absolute_file_name(path(problog), Problog, [access(exist), extensions([exe])]),		
	absolute_file_name(output, Modelo, [file_type(prolog)]),
	process_create(Problog, [Modelo], [stdout(pipe(In))]),
	read_string(In, _, Result),
	split_string(Result, "\n\t", "\r ", L),
	append(L1, [_], L),
	lista_valores(L1, ListaValores).
lista_valores([X, Y|T], [TermValor|T1]):-
	split_string(X, "", ":", [X1|_]), 
	term_string(TermX, X1), TermX =.. [calcular_patron, Dados, Patron, Categoria], 
	number_string(NumberY, Y), TermValor =.. [p, Dados, Patron, Categoria, NumberY], 
	lista_valores(T, T1).
lista_valores([],[]).

% Predicado para copiar el contenido de un archivo y agregar consultas al final
copiary_agregar_consultas(ArchivoOriginal, NuevoArchivo, Consultas) :-
	% Abre el archivo original en modo lectura

	open(ArchivoOriginal, read, StreamEntrada), % Abre el nuevo archivo en modo escritura
	
	open(NuevoArchivo, write, StreamSalida), % Copia el contenido
	
	copiar_contenido(StreamEntrada, StreamSalida), % Cierra el archivo original después de copiar el contenido
	
	close(StreamEntrada), % Agrega las consultas al nuevo archivo
	
	agregar_consultas(StreamSalida, Consultas), % Cierra el nuevo archivo después de agregar las consultas
	
	close(StreamSalida).

% Predicado para copiar el contenido de un stream a otro
copiar_contenido(StreamEntrada, StreamSalida) :-
	% Comprueba si hay más datos para leer en el stream de entrada

	 \+ at_end_of_stream(StreamEntrada), % Lee una línea del stream de entrada
		
	read_line_to_string(StreamEntrada, Linea), % Escribe la línea en el stream de salida
	
	writeln(StreamSalida, Linea), % Continúa copiando el resto del contenido
	
	copiar_contenido(StreamEntrada, StreamSalida).
copiar_contenido(_, _).

% Predicado para agregar consultas a un stream
agregar_consultas(StreamSalida, Consultas) :-
	% Para cada consulta en la lista, escribe en el stream de salida

	forall(
		member(Consulta, Consultas), 
		writeln(StreamSalida, Consulta)).
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
eleccion_slot(_, Tablero, ia_det, Categoria) :-
	categoria_superior(Categoria, N), 
	categoria_disponible(Tablero, Categoria), N < 4, !.

% Elijo la categoria disponible con mayor puntaje
eleccion_slot(Dados, Tablero, ia_det, Categoria) :-
	elegir_categoria(Dados, Tablero, Categoria).

eleccion_slot(Dados, Tablero, ia_prob, Categoria) :-
	elegir_categoria(Dados, Tablero, Categoria).

% Game Loop

% Jugador yahtzee
% Jugador puede ser humano o ia
yahtzeelog(Estrategia,Seed):-
    set_random(seed(Seed)),
    partida(Estrategia,TableroFinal),
    writeln('Termino el juego'),
    % Termina el juego, calculo los resultados.
    writeln(TableroFinal),
    puntaje_tablero(TableroFinal,PuntajeFinal),
    write('Puntaje obtenido:'),writeln(PuntajeFinal).

% Esto es simplemente para no utilizar ronda1 como sinónimo de juego
partida(Estrategia,TableroFinal):-
    inicial(Tablero),
    ronda(1,Estrategia,Tablero,TableroFinal).

% Ronda de juego
% NumRonda es el número de ronda
% Tablero es el Tablero hasta el momento
% TableroSalida es el Tablero una vez finalizada la ronda
ronda(L1,_,Tablero,Tablero):-
    categorias(C),
    length(C,L),
    L1 =:= L+1.

ronda(NumRonda,Estrategia,Tablero,TableroSalida):-
    categorias(C),length(C,L),
    NumRonda =< L,
    writeln('-----'),
    write('Ronda numero:'),
    writeln(NumRonda),
    writeln('Tablero actual:'),
    writeln(Tablero),
    lanzamiento([_,_,_,_,_],[1,1,1,1,1],Dados),
    write('Primer Lanzamiento:'),writeln(Dados),
    cambio_dados(Dados,Tablero,Estrategia,Patron),
    write('Patron sugerido:'),writeln(Patron),
    lanzamiento(Dados,Patron,Dados1),
    write('Segundo Lanzamiento:'),writeln(Dados1),
    cambio_dados(Dados1,Tablero,Estrategia,Patron1),
    write('Patron sugerido:'),writeln(Patron1),
    lanzamiento(Dados1,Patron1,Dados2),
    write('Tercer Lanzamiento:'),writeln(Dados2),
    eleccion_slot(Dados2,Tablero,Estrategia,Slot),
    write('Slot elegido:'),writeln(Slot),
    puntaje(Dados2,Slot,Punt),
    ajustar_tablero(Tablero,Slot,Punt,Tablero2),
    NumRonda1 is NumRonda +1, 
    writeln('Siguiente ronda...'),
    ronda(NumRonda1,Estrategia,Tablero2,TableroSalida).
