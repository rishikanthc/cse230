remove_all(_,[],[]).
remove_all(X,[X|T],[A|R]) :- remove_all(X,T,[A|R]), not(X=A).
remove_all(X,[A|T],[A|R]) :- remove_all(X,T,R), not(A=X).

remove_first(_,[],[]).
remove_first(X,[X|T],T).
remove_first(X,[A|T],[A|R]) :- remove_first(X,T,R), not(X=A).

prefix([],T).
prefix([A|B], [A|T]) :- prefix(B,T).

segment(A,B) :- prefix(A,B).
segment(X,[_|B]) :- segment(X,B).
