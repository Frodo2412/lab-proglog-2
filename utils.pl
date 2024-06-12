categorias_disponibles(Tablero, Categorias) :-
	findall(
		Categoria, 
		(member(s(Categoria, nil), Tablero)), 
		Categorias).

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
	absolute_file_name(
		path(problog), Problog, 
		[access(exist), 
		extensions([exe])]), 
	absolute_file_name(output, Modelo, 
		[file_type(prolog)]), 
	process_create(Problog, [Modelo], 
		[stdout(
			pipe(In))]), 
	read_string(In, _, Result), 
	split_string(Result, "\n\t", "\r ", L), 
	append(L1, [_], L), 
	lista_valores(L1, ListaValores).

% Predicado para obtener las categorías disponibles en el tablero
lista_valores([X, Y|T], [TermValor|T1]):-
	split_string(X, "", ":", [X1|_]), 
	term_string(TermX, X1), TermX =.. [calcular_patron, Dados, Patron, Categoria], 
	number_string(NumberY, Y), TermValor =.. [p, Dados, Patron, Categoria, NumberY], 
	lista_valores(T, T1).
lista_valores([],[]).

% Predicado para copiar el contenido de un archivo y agregar consultas al final
copiary_agregar_consultas(ArchivoOriginal, NuevoArchivo, Consultas) :-
	open(ArchivoOriginal, read, StreamEntrada),
	open(NuevoArchivo, write, StreamSalida),
	copiar_contenido(StreamEntrada, StreamSalida),
	close(StreamEntrada),
	agregar_consultas(StreamSalida, Consultas),
	close(StreamSalida).

% Predicado para copiar el contenido de un stream a otro
copiar_contenido(StreamEntrada, StreamSalida) :-
	 \+ at_end_of_stream(StreamEntrada),
	read_line_to_string(StreamEntrada, Linea), 
	writeln(StreamSalida, Linea),
	copiar_contenido(StreamEntrada, StreamSalida).
copiar_contenido(_, _).

% Predicado para agregar consultas a un stream
agregar_consultas(StreamSalida, Consultas) :-
	forall(member(Consulta, Consultas), writeln(StreamSalida, Consulta)).
