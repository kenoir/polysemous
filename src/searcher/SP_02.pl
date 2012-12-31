:- module(create_query_vector, [create_query_vector/3]).

%--------------------

create_query_vector(R,QT,QV):-
	append_atoms(R,'lib/senseref.txt',SP),
	sp02_read_senseref(SP,SL),
	sp02_create_index_vector(SL,QT,QV).

%--------------------

sp02_create_index_vector([],_,[]).
sp02_create_index_vector([[H,N]|T1], L, [V|T2]):-
	sp02_match_ref(H,L,X),
	V is X / N,
	sp02_create_index_vector(T1,L,T2).
sp02_create_index_vector([[H,_]|T1],L, [0|T2]):-
	not(sp02_match_ref(H,L,_)),
	sp02_create_index_vector(T1,L,T2).

sp02_match_ref(X,[[H,N]|_],N):-
	lc_equals(X,H).
sp02_match_ref(X,[[H,_]|T],N):-
	not(lc_equals(X,H)),
	sp02_match_ref(X,T,N).

%----------------------------

sp02_read_senseref(P,D):-
	open(P,read, _, [alias(input)]),
	sp02_rs_read_file(D,input), !,
	close(input).

sp02_rs_read_file(Y,input):-
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
	sp02_rs_do_add([],Z,[A,B,C,D,E,F,G,H,I,J],input),
	sp02_rs_pull_next(Z,Y,[A,B,C,D,E,F,G,H,I,J],input).

sp02_rs_pull_next(M,M,_,input):-
	peek_code(input, P),
	P == -1.

sp02_rs_pull_next(M,L,[_|T],input):-
	peek_code(input, P),
	not(P == -1),
	get_code(input, A),
	append(T,[A],W),
	sp02_rs_do_add(M,N,W,input),
	sp02_rs_pull_next(N,L,W,input).

sp02_rs_do_add(M,M,_,input):-
	peek_code(input,C),
	C == -1.

sp02_rs_do_add(O,O,T,input):-
	peek_code(input,C),
	not(C == -1),
	atom_codes(X,T),
	not(sp02_rs_is_stop_sequence(X)).

sp02_rs_do_add(O,L,T,input):-
	peek_code(input,C),
	not(C == -1),
	atom_codes(X,T),
	sp02_rs_is_stop_sequence(X),
	sp02_pull_ref(N,input),
	atom_codes(CA,N),
	get_code(input,_),
	get_code(input,_),
	get_code(input,_),
	get_code(input,_),
	get_code(input,_),
	get_code(input,_),
	get_code(input,_),
	get_code(input,_),
	sp02_pull_count(UVL,input),
	atom_codes(VA,UVL),
	term_to_atom(CVL,VA),
	append(O,[[CA,CVL]],L).

sp02_rs_is_stop_sequence(X):-
	X == '<ref code='.

sp02_pull_ref([],input):-
	peek_code(input,X),
	sp02_is_space(X).

sp02_pull_ref([H|T],input):-
	peek_code(input,H),
	not(sp02_is_space(H)),
	get_code(input,_),
	sp02_pull_ref(T,input).

sp02_pull_count([],input):-
	peek_code(input,X),
	is_end_tag(X).

sp02_pull_count([H|T],input):-
	peek_code(input,H),
	not(is_end_tag(H)),
	get_code(input,_),
	sp02_pull_count(T,input).

sp02_is_space(X):-
	X == 32.

%--------------------