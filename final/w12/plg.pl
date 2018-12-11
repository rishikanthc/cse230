sorted([]).
sorted([_]).
sorted([A,B|T]) :- A =< B, sorted([B|T]).

sortfn(L1,L2) :- permutation(L1,R), sorted(R),!, L2 = R.

split([],[],[]).
split([X],[X],[]).
split([X|T],[X|R],O) :- split(T,O,R).

merge([],L,L).
merge(L,[],L).
merge([X|T1],[Y|T2],[Y|R]) :- Y =< X, merge([X|T1],T2,R).
merge([X|T1],[Y|T2],[X|R]) :- X < Y, merge(T1,[Y|T2],R).

merge_sort([],[]).
merge_sort([X],[X]).
merge_sort(L,S) :- split(L,Left,Right),merge_sort(Left,L1),merge_sort(Right,R1),merge(L1,R1,S).
