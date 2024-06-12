cambio_dados_h(Dados, Tablero, Patron) :-
    writeln('Este es el tablero actual:'), 
	writeln(Tablero), 
	writeln('Estos son tus dados:'), 
	writeln(Dados), 
	leer_lista(0, Patron).

%%%%% Utility functions

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
	member(s(Categoria, nil), Tablero).
