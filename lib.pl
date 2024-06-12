% count(+Lista, +Elemento, -Cantidad) -> count(+Lista, ?Elemento, ?Cantidad)
count_aux(Lista, N, Acc, Acc) :-
    \+ select(N, Lista, _).
count_aux(Lista, N, Acc, Count) :-
   select(N, Lista, Resto), NewAcc is Acc + 1, 
   count_aux(Resto, N, NewAcc, Count).
count(Lista, N, Count) :-
   count_aux(Lista, N, 0, Count).
