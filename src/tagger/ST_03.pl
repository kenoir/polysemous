:- module(create_sense_matrix, [create_sense_matrix/1]).

%--------------------

create_sense_matrix(R):-
	append_atoms(R,'lib/senseref.txt',SP),
	append_atoms(R,'lib/sensemat.txt',MP),
	append_atoms(R,'lib/tdoc/*',TP),
	read_senseref(SP,SL),
	expand_file_name(TP,TPL),
	st03_obtain_index_vector(SL,TPL,MP).
	
st03_obtain_index_vector(_,[],_).
st03_obtain_index_vector(SL,[TP|T],MP):-
	obtain_document_senses(TP,DSL,TI),
	st03_format_document_sense_list(DSL,FDSL),
	st03_create_index_vector(SL,FDSL,V),
	tell(MP),
	write('<vector title='),
	write(TI),
	write('>'),
	write(V),
	write('</vector>'),
	told,	
	st03_st03_obtain_index_vector_tail(SL,T,MP).

st03_st03_obtain_index_vector_tail(_,[],_).
st03_st03_obtain_index_vector_tail(SL,[TP|T],MP):-
	obtain_document_senses(TP,DSL,TI),
	st03_format_document_sense_list(DSL,FDSL),
	st03_create_index_vector(SL,FDSL,V),
	append(MP),
	write('<vector title='),
	write(TI),
	write('>'),
	write(V),
	write('</vector>'),
	told,	
	st03_st03_obtain_index_vector_tail(SL,T,MP).

%--------------------

st03_format_document_sense_list([],[]).
st03_format_document_sense_list([[N1,S]|T1],[[N2,S]|T2]):-
	st03_number_occurences(T1,[N1,S],N2),
	st03_remove_from_list(T1,[N1,S],NT1),
	st03_format_document_sense_list(NT1,T2).
	
st03_remove_from_list([],_,[]).
st03_remove_from_list([[N1,S1]|T1],[N2,S2],[[N1,S1]|T2]):-
	not(S1 == S2),
	st03_remove_from_list(T1,[N2,S2],T2).
st03_remove_from_list([[_,S1]|T1],[N2,S2],T2):-
	S1 == S2,
	st03_remove_from_list(T1,[N2,S2],T2).

st03_number_occurences([],[N,_],N).
st03_number_occurences([[N1,S1]|T],[N2,S2],X):-
	S1 == S2,
	st03_number_occurences(T,[N2,S2],Y),
	X is Y + N1.
st03_number_occurences([[_,S1]|T],[N2,S2],N):-
	not(S1 == S2),
	st03_number_occurences(T,[N2,S2],N).

%--------------------

st03_create_index_vector([],_,[]).
st03_create_index_vector([[H,N]|T1], L, [V|T2]):-
	st03_match_ref(H,L,X),
	V is X / N,
	st03_create_index_vector(T1,L,T2).
st03_create_index_vector([[H,_]|T1],L, [0|T2]):-
	not(st03_match_ref(H,L,_)),
	st03_create_index_vector(T1,L,T2).

st03_match_ref(X,[[N,H]|_],N):-
	term_to_atom(XT,X),
	term_to_atom(HT,H),
	XT == HT.
st03_match_ref(X,[[_,H]|T],N):-
	not(X == H),
	st03_match_ref(X,T,N).

%--------------------
