:- module(run_search, [run_search/1]).

%--------------------

run_search(R):-
	obtain_query_terms(UQT),
	sp06_format_document_sense_list(UQT,FQT),
	create_query_vector(R,FQT,QV),
	cosine_coefficient(R,QV,CL),
	rank_results(CL,RL),
	display_list_results(RL),
	write('\n'),
	write('Another search? (y/n): '),
	read(X),
	write('\n'),
	sp06_search_again(X,R).
	
sp06_search_again(X,R):-
	not(sp06_valid_choice(X)),
	write('\n'),
	write('That is not a valid choice, please type y or n:'),
	read(Y),
	sp06_search_again(Y,R).
sp06_search_again(X,R):-
	sp06_valid_choice(X),
	X == 'y',
	run_search(R).
sp06_search_again(X,_):-
	sp06_valid_choice(X),
	X == 'n',
	halt.

sp06_valid_choice(X):-
	X == 'y'.
sp06_valid_choice(X):-
	X == 'n'.

%--------------------

sp06_format_document_sense_list([],[]).
sp06_format_document_sense_list([H|T1],[[H,N]|T2]):-
	number_occurences(T1,H,N),
	sp06_remove_from_list(T1,H,NT1),
	sp06_format_document_sense_list(NT1,T2).
	
sp06_remove_from_list([],_,[]).
sp06_remove_from_list([H|T1],X,[H|T2]):-
	not(H == X),
	sp06_remove_from_list(T1,X,T2).
sp06_remove_from_list([H|T1],X,T2):-
	H == X,
	sp06_remove_from_list(T1,X,T2).

%--------------------

