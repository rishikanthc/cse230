link(san_diego, seattle).
link(seattle, dallas).
link(dallas, new_york).
/* commenting this to test for cycles */
/*link(new_york, chicago).*/
link(new_york, seattle).
link(chicago, boston).
link(boston, san_diego).

/* Q5.a */
path_2(X,Y) :- link(X, A), link(A, Y).

/* Q5.b */
path_3(A,B) :- path_2(A, X), link(X,B).

/* Q5.c */
path_N(A,B,N) :- link(A, B), N=1.
path_N(A,B,N) :- N > 1, R is N - 1, path_N(A,C,R), path_N(C,B,1).

/* Q5.d */
path(A, B) :- path_helper(A, B, [A]).
path_helper(A, B, Seen) :- link(A,B), not(member(B, Seen)).
path_helper(A, B, Seen) :- link(A,C), not(member(C, Seen)), path_helper(C,B,[C|Seen]).

