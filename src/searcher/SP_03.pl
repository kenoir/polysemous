:- module(cosine_coefficient, [cosine_coefficient/3]).

%--------------------

cosine_coefficient(R,Q,L):-
	append_atoms(R, 'lib/sensemat.txt',P),
	sp03_obtain_index_vector(P,D),
	sp03_obtain_cosine_coefficient(D,Q,L).

sp03_obtain_cosine_coefficient([],_,[]).
sp03_obtain_cosine_coefficient([[TI,D]|T1],Q,[[TH,TI]|T2]):-
	sp03_calculate_cosine_coefficient(D,Q,TH),
	sp03_obtain_cosine_coefficient(T1,Q,T2).

sp03_obtain_index_vector(P,D):-
	open(P,read, _, [alias(input)]),
	sp03_create_document_matrix(D,input), !,
	close(input).

sp03_create_document_matrix(Y,input):-
	get_code(input, A),
	get_code(input, B),
	get_code(input, C),
	get_code(input, D),
	get_code(input, E),
	get_code(input, F),
	get_code(input, G),
	get_code(input, H),
	get_code(input, I),
	get_code(input, J),
	get_code(input, K),
	get_code(input, L),
	get_code(input, M),
	get_code(input, N),
	sp03_sd_do_add([],Z,[A,B,C,D,E,F,G,H,I,J,K,L,M,N],input),
	sp03_sd_pull_next(Z,Y,[A,B,C,D,E,F,G,H,I,J,K,L,M,N],input).

sp03_sd_pull_next(M,L,[_|T],input):-
	peek_code(input, P),
	not(P == -1),
	get_code(input, A),
	append(T,[A],W),
	sp03_sd_do_add(M,N,W,input),
	sp03_sd_pull_next(N,L,W,input).

sp03_sd_pull_next(M,M,_,input):-
	peek_code(input, P),
	P == -1.

sp03_sd_do_add(O,L,T,input):-
	atom_codes(X,T),
	sp03_sd_is_stop_sequence(X),
	sp03_pull_title(N,input),
	atom_codes(C,N),
	get_code(input,_),
	sp03_pull_vector(UVL,input),
	atom_codes(VA,UVL),
	term_to_atom(CVL,VA),
	append(O,[[C,CVL]],L).

sp03_sd_do_add(O,O,T,input):-
	atom_codes(X,T),
	not(sp03_sd_is_stop_sequence(X)).

sp03_pull_vector([],input):-
	peek_code(input,X),
	sp03_is_start_tag(X).

sp03_pull_vector([H|T],input):-
	peek_code(input,H),
	not(sp03_is_start_tag(H)),
	get_code(input,_),
	sp03_pull_vector(T,input).

sp03_pull_title([],input):-
	peek_code(input,X),
	is_end_tag(X).

sp03_pull_title([H|T],input):-
	peek_code(input,H),
	not(is_end_tag(H)),
	get_code(input,_),
	sp03_pull_title(T,input).

sp03_is_start_tag(X):-
	X == 60.

sp03_sd_is_stop_sequence(X):-
	X == '<vector title='.

sp03_is_vector_char(X):-
	X > 47,
	X < 58.
sp03_is_vector_char(X):-
	X == 46.


%--------------------

sp03_calculate_cosine_coefficient(X,Y,Z):-
	sp03_dot_product(X,Y,A),
	sp03_vector_sum(A,TopTerm),
	sp03_dot_product(X,X,B),
	sp03_vector_sum(B,BSum),
	sp03_dot_product(Y,Y,C),
	sp03_vector_sum(C,CSum),
	BCSum is BSum * CSum,
	BotTerm is sqrt(BCSum),
	sp03_check_no_zero_divisor(TopTerm, BotTerm, Z).

sp03_check_no_zero_divisor(_,0,0).
sp03_check_no_zero_divisor(T,B,R):-
	not(B == 0),
	R is T / B.

sp03_dot_product([],[],[]).
sp03_dot_product([H1|T1],[H2|T2],[H|T]):-
	H is H1 * H2,
	sp03_dot_product(T1,T2,T).

sp03_vector_sum([],0).
sp03_vector_sum([H|T],X):-
	sp03_vector_sum(T,Y),
	X is Y + H.

%--------------------
