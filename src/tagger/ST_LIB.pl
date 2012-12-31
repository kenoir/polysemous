%----------------------------

exists(X,[X|_]).
exists(X,[H|T]):-
	not(X == H),
	exists(X,T).

remove_duplicates([],[]).
remove_duplicates([H|T1], [H|T2]):-
	not(exists(H,T1)),
	remove_duplicates(T1,T2).

remove_duplicates([H|T1], T2):-
	exists(H,T1),
	remove_duplicates(T1,T2).

lc_exists(X,[H|_]):-
	atom_codes(X,AX),
	atom_codes(H,AH),
	AX == AH.
	
lc_exists(X,[H|T]):-
	atom_codes(X,AX),
	atom_codes(H,AH),
	not(AX == AH),
	lc_exists(X,T).

lc_equals(X,H):-
	atom_codes(X,AX),
	atom_codes(H,AH),
	AX == AH.

stick_on([],L1,L1).
stick_on([H|T],L1,[H|L2]):-
	stick_on(T,L1,L2).

is_word_valid(W):-
	s(_,_,W,'n',_,_).

clean_list([],[]).
clean_list([H|T],T1):-
	H == '_',
	clean_list(T,T1).

clean_list([H|T],[H|T1]):-
	not(H == '_'),
	clean_list(T,T1).

append_atoms(X,Y,Z):-
	string_to_atom(SX,X),
	string_to_list(SX,LX),
	string_to_atom(SY,Y),
	string_to_list(SY,LY),
	append(LX,LY,LZ),
	string_to_list(SZ,LZ),
	string_to_atom(SZ,Z).

%--------------------


read_senseref(P,D):-
	open(P,read, _, [alias(input)]),
	rs_read_file(D,input), !,
	close(input).

rs_read_file(Y,input):-
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
	rs_do_add([],Z,[A,B,C,D,E,F,G,H,I,J],input),
	rs_pull_next(Z,Y,[A,B,C,D,E,F,G,H,I,J],input).

rs_pull_next(M,M,_,input):-
	peek_code(input, P),
	P == -1.

rs_pull_next(M,L,[_|T],input):-
	peek_code(input, P),
	not(P == -1),
	get_code(input, A),
	append(T,[A],W),
	rs_do_add(M,N,W,input),
	rs_pull_next(N,L,W,input).

rs_do_add(M,M,_,input):-
	peek_code(input,C),
	C == -1.

rs_do_add(O,O,T,input):-
	peek_code(input,C),
	not(C == -1),
	atom_codes(X,T),
	not(rs_is_stop_sequence(X)).

rs_do_add(O,L,T,input):-
	peek_code(input,C),
	not(C == -1),
	atom_codes(X,T),
	rs_is_stop_sequence(X),
	pull_ref(N,input),
	atom_codes(CA,N),
	get_code(input,_),
	get_code(input,_),
	get_code(input,_),
	get_code(input,_),
	get_code(input,_),
	get_code(input,_),
	get_code(input,_),
	get_code(input,_),
	pull_count(UVL,input),
	atom_codes(VA,UVL),
	term_to_atom(CVL,VA),
	append(O,[[CA,CVL]],L).

rs_is_stop_sequence(X):-
	X == '<ref code='.

pull_ref([],input):-
	peek_code(input,X),
	is_space(X).

pull_ref([H|T],input):-
	peek_code(input,H),
	not(is_space(H)),
	get_code(input,_),
	pull_ref(T,input).

pull_count([],input):-
	peek_code(input,X),
	is_end_tag(X).

pull_count([H|T],input):-
	peek_code(input,H),
	not(is_end_tag(H)),
	get_code(input,_),
	pull_count(T,input).

is_space(X):-
	X == 32.

is_end_tag(X):-
	X == 62.

%----------------------------

write_senseref(P,[H|T]):-
	tell(P),
	write_sense_dat(H),
	told,
	create_senseref(T,P).

create_senseref([],_).
create_senseref([H|T],P):-
	append(P),
	write_sense_dat(H),
	told,
	create_senseref(T,P).

write_sense_dat([R,_]):-
	write('<ref code='),
	write(R),
	write(' weight='),
	write('1'),
	write('></ref>').


%----------------------------

obtain_document_senses(P,D,T):-
	file_base_name(P,T),
	open(P,read, _, [alias(input)]),
	create_document_sense_list(D,input), !,
	close(input).

create_document_sense_list(L,input):-
	get_code(input, A),
	get_code(input, B),
	get_code(input, C),
	get_code(input, D),
	get_code(input, E),
	get_code(input, F),
	get_code(input, G),
	get_code(input, H),
	do_add([],M,[A,B,C,D,E,F,G,H],input),
	pull_next(M,L,[A,B,C,D,E,F,G,H],input).

pull_next(M,L,[_|T],input):-
	peek_code(input, P),
	not(P == -1),
	get_code(input, A),
	append(T,[A],W),
	do_add(M,N,W,input),
	pull_next(N,L,W,input).

pull_next(M,M,_,input):-
	peek_code(input, P),
	P == -1.

do_add(O,L,T,input):-
	atom_codes(X,T),
	is_stop_sequence(X),
	pull_number(N,input),
	atom_codes(C,N),
	term_to_atom(SL,C),
	append(O,SL,L).

do_add(O,O,T,input):-
	atom_codes(X,T),
	not(is_stop_sequence(X)).

pull_number([],input):-
	peek_code(input,X),
	is_end_char(X).

pull_number([H|T],input):-
	peek_code(input,H),
	not(is_end_char(H)),
	get_code(input,_),
	pull_number(T,input).

is_stop_sequence(X):-
	X == 'synvec="'.

is_end_char(X):-
	X == 34.

%----------------------------