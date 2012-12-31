:- module(display_list_results, [display_list_results/1]).

%--------------------

display_list_results([]).
display_list_results([H|T]):-
	extract_number(H,N),
	extract_value(H,V),
	write(N),
	write(' '),
	write(V),
	write('\n'),
	display_list_results(T).

%--------------------