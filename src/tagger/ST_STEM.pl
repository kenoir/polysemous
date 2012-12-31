:- module(stem, [stem/2]).

%--------------------

stem(A,B):-
	%ensure_loaded('../wordnet/wn_s.pl'),
	ststem_stem_noun(A,B).

ststem_stem_noun(A,B):-
	ststem_noun_regular_none(A,B).
ststem_stem_noun(A,B):-
	ststem_noun_regular_ies(A,B).
ststem_stem_noun(A,B):-
	ststem_noun_regular_s(A,B).
ststem_stem_noun(A,B):-
	ststem_noun_regular_ses(A,B).
ststem_stem_noun(A,B):-
	ststem_noun_regular_men(A,B).
ststem_stem_noun(A,B):-
	ststem_noun_regular_ches(A,B).
ststem_stem_noun(A,B):-
	ststem_noun_regular_zes(A,B).
ststem_stem_noun(A,B):-
	ststem_noun_regular_xes(A,B).
ststem_stem_noun(A,B):-
	ststem_read_exc('../wordnet/noun.exc',EL),
	ststem_noun_irregular(A,EL,B).

%-----------------------------------

ststem_noun_irregular(W,[[W,B]|_],B).
ststem_noun_irregular(W,[[A,_]|T],L):-
	not(W == A),
	ststem_noun_irregular(W,T,L).

%-----------------------------------

ststem_read_exc(P,EL):-
	open(P,read, _, [alias(input)]),
	ststem_create_exception_list(EL, input),
	close(input), !.

ststem_create_exception_list([],input):-
	peek_code(input, Y),
	Y == -1.
ststem_create_exception_list(X,input):-
	peek_code(input, Y),
	not(Y == -1),
	ststem_read_line(X, input).

ststem_read_line([],input):-
	peek_code(input, X),
	X == -1.
ststem_read_line([[A,B]|T],input):-
	peek_code(input, X),
	not(X == -1),
	ststem_read_unstemmed(input,AC),
	atom_codes(A,AC),
	ststem_read_stemmed(input,BC),
	atom_codes(B,BC),
	ststem_read_line(T,input).

ststem_read_unstemmed(input,[]):-
	peek_code(input, X),
	X == -1.
ststem_read_unstemmed(input,[]):-
	peek_code(input, X),
	X == 32,
	get_code(input,_).
ststem_read_unstemmed(input,[X|T]):-
	peek_code(input, X),
	not(X == 32),
	not(X == -1),
	get_code(input,_),
	ststem_read_unstemmed(input,T).

ststem_read_stemmed(input,[]):-
	peek_code(input, X),
	X == -1.
ststem_read_stemmed(input,[]):-
	peek_code(input, X),
	X == 10,
	get_code(input,_).
ststem_read_stemmed(input,[X|T]):-
	peek_code(input, X),
	not(X == 10),
	not(X == -1),
	get_code(input,_),
	ststem_read_stemmed(input,T).

%-----------------------------------

ststem_noun_regular_none(A,A):-
	ststem_noun_exists(A).		

ststem_noun_regular_ies(A,H):-
	ststem_word_split(3,A,PH,B),
	B == 'ies',
	atom_concat(PH,'y',H),
	ststem_noun_exists(H).	

ststem_noun_regular_ses(A,H):-
	ststem_word_split(3,A,PH,B),
	B == 'ses',
	atom_concat(PH,'s',H),
	ststem_noun_exists(H).	

ststem_noun_regular_s(A,H):-
	ststem_word_split(1,A,H,B),
	B == 's',
	ststem_noun_exists(H).	

ststem_noun_regular_men(A,H):-
	ststem_word_split(3,A,PH,B),
	B == 'men',
	atom_concat(PH,'man',H),
	ststem_noun_exists(H).	

ststem_noun_regular_shes(A,H):-
	ststem_word_split(4,A,PH,B),
	B == 'shes',
	atom_concat(PH,'sh',H),
	ststem_noun_exists(H).	

ststem_noun_regular_ches(A,H):-
	ststem_word_split(3,A,PH,B),
	B == 'ches',
	atom_concat(PH,'ch',H),
	ststem_noun_exists(H).	

ststem_noun_regular_zes(A,H):-
	ststem_word_split(3,A,PH,B),
	B == 'zes',
	atom_concat(PH,'z',H),
	ststem_noun_exists(H).	

ststem_noun_regular_xes(A,H):-
	ststem_word_split(3,A,PH,B),
	B == 'xes',
	atom_concat(PH,'x',H),
	ststem_noun_exists(H).	

%-----------------------------------

ststem_noun_exists(A):-
	s(_,_,A,'n',_,_), !.

%-----------------------------------
	
ststem_word_split(N,W,H,T):-
	ststem_disassemble_string(W,LW),
	ststem_pull_tail(N,LW,LH,LT),
	ststem_reassemble_string(LH,SH),
	ststem_reassemble_string(LT,ST),
	string_to_atom(SH,H),
	string_to_atom(ST,T).

ststem_pull_tail(N,L,H,T):-
	ststem_reverse_list(L,RL),
	ststem_pull_head(N,RL,RH,RT),
	reverse(RH,T),
	reverse(RT,H).

ststem_pull_head(0,L,[],L).
ststem_pull_head(N,[H|T1],[H|T2],L):-
	N > 0,
	M is N - 1,
	ststem_pull_head(M,T1,T2,L).

ststem_reverse_list([],[]).
ststem_reverse_list([H|T],L):-
	reverse(T,M),
	append(M,[H],L).

ststem_reassemble_string(A,B):-
	ststem_list_to_char(L,A),
	string_to_list(B,L).

ststem_disassemble_string(A,B):-
	string_to_list(A,L),
	ststem_list_to_char(L,B).

ststem_list_to_char([],[]).
ststem_list_to_char([H1|T1],[H2|T2]):-
	char_code(H2,H1),
	ststem_list_to_char(T1,T2).

%-----------------------------------