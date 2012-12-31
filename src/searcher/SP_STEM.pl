:- module(stem, [stem/2]).

%--------------------

stem(A,B):-
	%load_module('../wordnet/wn_s.pl'),
	spstem_stem_noun(A,B).

spstem_stem_noun(A,B):-
	spstem_noun_regular_none(A,B).
spstem_stem_noun(A,B):-
	spstem_noun_regular_ies(A,B).
spstem_stem_noun(A,B):-
	spstem_noun_regular_s(A,B).
spstem_stem_noun(A,B):-
	spstem_noun_regular_ses(A,B).
spstem_stem_noun(A,B):-
	spstem_noun_regular_men(A,B).
spstem_stem_noun(A,B):-
	spstem_noun_regular_ches(A,B).
spstem_stem_noun(A,B):-
	spstem_noun_regular_zes(A,B).
spstem_stem_noun(A,B):-
	spstem_noun_regular_xes(A,B).
spstem_stem_noun(A,B):-
	spstem_read_exc('../wordnet/noun.exc',EL),
	spstem_noun_irregular(A,EL,B).

%-----------------------------------

spstem_noun_irregular(W,[[W,B]|_],B).
spstem_noun_irregular(W,[[A,_]|T],L):-
	not(W == A),
	spstem_noun_irregular(W,T,L).

%-----------------------------------

spstem_read_exc(P,EL):-
	open(P,read, _, [alias(input)]),
	spstem_create_exception_list(EL, input),
	close(input), !.

spstem_create_exception_list([],input):-
	peek_code(input, Y),
	Y == -1.
spstem_create_exception_list(X,input):-
	peek_code(input, Y),
	not(Y == -1),
	spstem_read_line(X, input).

spstem_read_line([],input):-
	peek_code(input, X),
	X == -1.
spstem_read_line([[A,B]|T],input):-
	peek_code(input, X),
	not(X == -1),
	spstem_read_unstemmed(input,AC),
	atom_codes(A,AC),
	spstem_read_stemmed(input,BC),
	atom_codes(B,BC),
	spstem_read_line(T,input).

spstem_read_unstemmed(input,[]):-
	peek_code(input, X),
	X == -1.
spstem_read_unstemmed(input,[]):-
	peek_code(input, X),
	X == 32,
	get_code(input,_).
spstem_read_unstemmed(input,[X|T]):-
	peek_code(input, X),
	not(X == 32),
	not(X == -1),
	get_code(input,_),
	spstem_read_unstemmed(input,T).

spstem_read_stemmed(input,[]):-
	peek_code(input, X),
	X == -1.
spstem_read_stemmed(input,[]):-
	peek_code(input, X),
	X == 10,
	get_code(input,_).
spstem_read_stemmed(input,[X|T]):-
	peek_code(input, X),
	not(X == 10),
	not(X == -1),
	get_code(input,_),
	spstem_read_stemmed(input,T).

%-----------------------------------

spstem_noun_regular_none(A,A):-
	spstem_noun_exists(A).		

spstem_noun_regular_ies(A,H):-
	spstem_word_split(3,A,PH,B),
	B == 'ies',
	atom_concat(PH,'y',H),
	spstem_noun_exists(H).	

spstem_noun_regular_ses(A,H):-
	spstem_word_split(3,A,PH,B),
	B == 'ses',
	atom_concat(PH,'s',H),
	spstem_noun_exists(H).	

spstem_noun_regular_s(A,H):-
	spstem_word_split(1,A,H,B),
	B == 's',
	spstem_noun_exists(H).	

spstem_noun_regular_men(A,H):-
	spstem_word_split(3,A,PH,B),
	B == 'men',
	atom_concat(PH,'man',H),
	spstem_noun_exists(H).	

spstem_noun_regular_shes(A,H):-
	spstem_word_split(4,A,PH,B),
	B == 'shes',
	atom_concat(PH,'sh',H),
	spstem_noun_exists(H).	

spstem_noun_regular_ches(A,H):-
	spstem_word_split(3,A,PH,B),
	B == 'ches',
	atom_concat(PH,'ch',H),
	spstem_noun_exists(H).	

spstem_noun_regular_zes(A,H):-
	spstem_word_split(3,A,PH,B),
	B == 'zes',
	atom_concat(PH,'z',H),
	spstem_noun_exists(H).	

spstem_noun_regular_xes(A,H):-
	spstem_word_split(3,A,PH,B),
	B == 'xes',
	atom_concat(PH,'x',H),
	spstem_noun_exists(H).	

%-----------------------------------

spstem_noun_exists(A):-
	s(_,_,A,'n',_,_), !.

%-----------------------------------
	
spstem_word_split(N,W,H,T):-
	spstem_disassemble_string(W,LW),
	spstem_pull_tail(N,LW,LH,LT),
	spstem_reassemble_string(LH,SH),
	spstem_reassemble_string(LT,ST),
	string_to_atom(SH,H),
	string_to_atom(ST,T).

spstem_pull_tail(N,L,H,T):-
	spstem_reverse_list(L,RL),
	spstem_pull_head(N,RL,RH,RT),
	reverse(RH,T),
	reverse(RT,H).

spstem_pull_head(0,L,[],L).
spstem_pull_head(N,[H|T1],[H|T2],L):-
	N > 0,
	M is N - 1,
	spstem_pull_head(M,T1,T2,L).

spstem_reverse_list([],[]).
spstem_reverse_list([H|T],L):-
	reverse(T,M),
	append(M,[H],L).

spstem_reassemble_string(A,B):-
	spstem_list_to_char(L,A),
	string_to_list(B,L).

spstem_disassemble_string(A,B):-
	string_to_list(A,L),
	spstem_list_to_char(L,B).

spstem_list_to_char([],[]).
spstem_list_to_char([H1|T1],[H2|T2]):-
	char_code(H2,H1),
	spstem_list_to_char(T1,T2).

%-----------------------------------