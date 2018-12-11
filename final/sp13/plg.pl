zip([],[],[]).
zip([X|L1],[Y|L2],[[X,Y]|R]) :- zip(L1,L2,R).

part([],_,[],[]).
part([X|T],P,[X|R1],R2) :- X=<P, part(T,P,R1,R2).
part([X|T],P,R1,[X|R2]) :- X>P, part(T,P,R1,R2).

qsort([],[]).
qsort([A|T],R) :- part(T,A,R1,R2), qsort(R1,Left), qsort(R2,Right), append(Left,[A|Right],R).
