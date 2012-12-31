:- module(rank_results, [rank_results/2]).

%--------------------

rank_results('_','_').
rank_results([],[]).
rank_results(A,B):-
	sp04_qsort(A,B).

sp04_qsort([],[]).
sp04_qsort([H|T], L2):-
	extract_number(H,N),
	sp04_partition(N,T,Littles,Bigs),
	sp04_qsort(Littles,SLittles),
	sp04_qsort(Bigs,SBigs),
	append(SBigs, [H|SLittles], L2).

sp04_partition(_,[],[],[]).
sp04_partition(O, [H|T], [H|LT], Bigs):-
	extract_number(H,N),
	O >= N,
	sp04_partition(O, T, LT, Bigs).
sp04_partition(O, [H|T], Littles, [H|BT]):-
	extract_number(H,N),
	O < N,
	sp04_partition(O, T, Littles, BT).

%--------------------