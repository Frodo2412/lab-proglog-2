% puntaje_tablero(+Tablero, -Puntaje)
puntaje_tablero(Tablero, Puntaje) :-
	partition(functor_superior, Tablero, Superiores, Inferiores), 
	maplist(extraer_puntaje, Superiores, PuntajesSuperiores), 
	sumlist(PuntajesSuperiores, PuntajeSuperior), 
	maplist(extraer_puntaje, Inferiores, PuntajesInferiores), 
	sumlist(PuntajesInferiores, PuntajeInferior), 
	(PuntajeSuperior >= 63 ->
		Puntaje is PuntajeSuperior + PuntajeInferior + 35;
		Puntaje is PuntajeSuperior + PuntajeInferior).

% ajustar_tablero(+Tablero,+Categoria,+Puntaje,-TableroSalida)
ajustar_tablero([], _, _, []).
ajustar_tablero([s(Categoria, Puntaje)|RestoTablero], Categoria, Puntaje, [s(Categoria, Puntaje)|RestoTablero]).
ajustar_tablero([s(Categoria, Puntaje)|RestoTablero], Objetivo, PuntajeObjetivo, [s(Categoria, Puntaje)|RestoTableroSalida]) :-
	Categoria \= Objetivo, 
	ajustar_tablero(RestoTablero, Categoria, PuntajeObjetivo, RestoTableroSalida).

% categoria_disponible(+Tablero, +Categoria)
categoria_disponible(Tablero, Categoria) :-
	member(
		s(Categoria, nil), Tablero).

% puntaje_categoria(+Tablero, +Categoria, -Puntaje)
puntaje_categoria(Tablero, Categoria, Puntaje) :-
	member(
		s(Categoria, Puntaje), Tablero).

%%%%% Utility functions

% categoria_superior(+Categoria)
categoria_superior(aces).
categoria_superior(twos).
categoria_superior(threes).
categoria_superior(fours).
categoria_superior(fives).
categoria_superior(sixes).

% functor_superior(+s(Categoria, _))
functor_superior(s(Categoria, _)) :-
	categoria_superior(Categoria).

% extraer_puntaje(+s(_, Puntaje), -Puntaje)
extraer_puntaje(s(_, Puntaje), Puntaje).
