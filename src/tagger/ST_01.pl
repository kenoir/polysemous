:- module(obtain_tag_document_set_members, [obtain_tag_document_set_members/1]).

%--------------------

obtain_tag_document_set_members(R):-
	append_atoms(R,'lib/tdoc/',TD),
	append_atoms(R,'lib/udoc/',UD),
	append_atoms(R,'lib/senseref.txt',SR),
	st01_obtain_document_list(TD,UD,UDL),
	st01_create_tag_document_set_members(UDL,SR,TD), !.

st01_create_tag_document_set_members([],_,_).
st01_create_tag_document_set_members([P|T],S,PT):-
	st01_obtain_sentence_list(P,Y),
	st01_cut_sentence_list(Y,X),
	st01_obtain_local_sense_list(X,Z), !,
	st01_write_tagged_file(PT,P,Y,X,Z),
	st01_conjoin_raw_senses(Z,A),
	read_senseref(S,SL),
	st01_add_senselist(SL,A,NSL),
	write_senseref(S,NSL),
	st01_create_tag_document_set_members(T,S,PT).

%--------------------

st01_write_tagged_file(P,N,Y,X,Z):-
	file_base_name(N,M),
	append_atoms(P,M,FP),
	st01_write_sentence(FP,Y,X,Z).	

%--------------------

st01_write_sentence(_,[],[],[]).
st01_write_sentence(P,[H|T],[LH|LT],[SH|ST]):-
	st01_write_word(P,H,LH,SH),
	append(P),
	write('<punc>.</punc>'),
	told,
	st01_write_sentence(P,T,LT,ST).

st01_write_word(_,[],[],[]).
st01_write_word(P,[' '|T],[_|LT],[_|ST]):-
	st01_write_word(P,T,LT,ST).
st01_write_word(P,['\n'|T],[_|LT],['_'|ST]):-
	st01_write_word(P,T,LT,ST).
st01_write_word(P,[H|T],[_|LT],['_'|ST]):-
	not(H == ' '),
	not(H == '/n'),
	atom_codes(H,X),
	st01_is_letter(X),
	append(P),
	write('<wf cmd=ignore>'),
	write(H),
	write('</wf>'),
	told,	
	st01_write_word(P,T,LT,ST).
st01_write_word(P,[H|T],[LH|LT],[SH|ST]):-
	not(SH == '_'),
	atom_codes(H,X),
	st01_is_letter(X),
	append(P),
	write('<wf cmd=done pos=NN lemma='),
	write(LH),
	write(' synvec="'),
	write(SH),
	write('">'),
	write(H),
	write('</wf>'),
	told,	
	st01_write_word(P,T,LT,ST).
st01_write_word(P,[H|T],[_|LT],['_'|ST]):-
	atom_codes(H,X),
	not(st01_is_letter(X)),
	not(H == ' '),
	not(H == '/n'),
	append(P),
	write('<punc>'),
	write(H),
	write('</punc>'),
	told,	
	st01_write_word(P,T,LT,ST).

st01_is_letter([X]):-
	X > 64,
	X < 91.
st01_is_letter([X]):-
	X > 96,
	X < 123.
st01_is_letter([X|_]):-
	X > 64,
	X < 91.
st01_is_letter([X|_]):-
	X > 96,
	X < 123.

%--------------------

st01_obtain_local_sense_list(L,S):-
	st01_obtain_refined_hood_list(L,R),
	st01_obtain_no_duplicates_context_list(R,P),
	st01_create_local_sense_list(P,S). 

st01_create_local_sense_list([],[]).
st01_create_local_sense_list([H|T],[SH|ST]):-
	st01_strip_conjoin_context_chl(H,CL),!,
	st01_remove_top_element(H,CL,SH),
	st01_create_local_sense_list(T,ST).

%--------------------

st01_obtain_no_duplicates_context_list(X,N):-
	st01_obtain_context_list(X,Y),
	st01_remove_duplicates_in_context(Y,N).

st01_remove_duplicates_in_context([],[]).
st01_remove_duplicates_in_context([H|T1],[R|T2]):-
	st01_split_context(H,R),
	st01_remove_duplicates_in_context(T1,T2).

st01_split_context([H|[T]],X):-
	st01_remove_h_from_t(H,T,R),
	append([H],[R],X).

%--------------------

st01_obtain_document_list(T,U,L):-
	string_to_atom(S,U),
	string_to_list(S,A),
	append(A,[42],B),
	string_to_list(C,B),
	expand_file_name(C,M),
	st01_whittle_document_list(T,M,L).

st01_whittle_document_list(_,[],[]).
st01_whittle_document_list(TP,[H|T1],[H|T2]):-
	file_base_name(H,N),
	append_atoms(TP,N,TH),
	not(exists_file(TH)),
	st01_whittle_document_list(TP,T1,T2).
st01_whittle_document_list(TP,[H|T1],L):-
	file_base_name(H,N),
	append_atoms(TP,N,TH),
	exists_file(TH),
	st01_whittle_document_list(TP,T1,L).	

%--------------------

st01_obtain_sentence_list(P,X):-
	open(P,read, _, [alias(input)]),
	st01_create_sentence_list(X, input),
	close(input), !.

st01_create_sentence_list([],input):-
	peek_code(input, Y),
	Y == -1.

st01_create_sentence_list([X|T],input):-
	st01_read_sentence(X, input),
	st01_create_sentence_list(T,input).

st01_read_sentence([],input):-
	peek_code(input, X),
	X == 46,
	get_code(input,_).

st01_read_sentence([],input):-
	peek_code(input, X),
	X == -1.

st01_read_sentence([W|T],input):-
	peek_code(input, X),
	st01_is_punctuation(X),
	atom_codes(W,[X]),
	get_code(input,_),
	st01_read_sentence(T,input).

st01_read_sentence([P|T],input):-
	peek_code(input, X),
	not(st01_is_punctuation(X)),
	not(X == 46),
	not(X == -1),
	st01_read_word(L,input),
	atom_codes(P,L),
	st01_read_sentence(T,input).

st01_read_word([],input):-
	peek_code(input,X),
	st01_is_punctuation(X).

st01_read_word([],input):-
	peek_code(input,X),
	X == 46.

st01_read_word([],input):-
	peek_code(input,X),
	X == -1.

st01_read_word([X|T],input):-
	peek_code(input,X),
	not(st01_is_punctuation(X)),
	not(X == 46),
	not(X == -1),
	get_code(input,_),
	st01_read_word(T,input).

st01_is_punctuation(X):-
	X > 0,
	X < 46.

st01_is_punctuation(X):-
	X > 46,
	X < 65.

st01_is_punctuation(X):-
	X > 90,
	X < 97.

%--------------------

st01_cut_sentence_list([],[]).
st01_cut_sentence_list([H|T],[H2|T2]):-
	st01_cut_word_list(H,H2),
	st01_cut_sentence_list(T,T2).
	
%--------------------

st01_cut_word_list([],[]).
st01_cut_word_list([H|T],[H|T2]):-
	is_word_valid(H),
	st01_cut_word_list(T,T2),!.
st01_cut_word_list([H|T],[S|T2]):-
	not(is_word_valid(H)),
	stem(H,S),
	is_word_valid(S),
	st01_cut_word_list(T,T2),!.
st01_cut_word_list([H|T],['_'|T2]):-
	not(is_word_valid(H)),
	st01_cut_word_list(T,T2).
	
%--------------------

st01_remove_top_element([['_'|T]|_],B,['_'|ST]):-
	st01_remove_body_element(['_'],[T|_],B,ST).

st01_remove_top_element([[H|T]|_],B,[SH|ST]):-
	st01_compare_hoods(H,T,B,SH),
	st01_remove_body_element([H],[T|_],B,ST).

st01_remove_body_element(_,[[   ],_],_,[]).
st01_remove_body_element(O,[[H|T]|_],B,[SH|ST]):-
	append(O,T,R),
	st01_compare_hoods(H,R,B,SH),
	append(O,[H],L),
	st01_remove_body_element(L,[T|_],B,ST).

%--------------------

st01_compare_hoods(H,B,C,T):-
	st01_remove_h_from_t(H,B,B1),
	st01_strip_conjoin_body_chl(B1,X), !,
	append(X,C,Y),
	st01_search_calc_hoods(H,Y,S),
	st01_adjust_scores(S,AS),
	st01_build_score_list(AS,H,SL),
	st01_rank_results(SL,T).

%--------------------

st01_adjust_scores('_','_').
st01_adjust_scores(S,AS):-
	maplist(st01_add(0.1),S,S1),
	st01_sum_list(S1,N),
	maplist(st01_divide(N),S1,AS).

st01_sum_list([],0).
st01_sum_list([H|T],N):-
	st01_sum_list(T,M),
	N is H + M.

st01_divide(A,B,C):-
	C is B / A.
st01_add(A,B,C):-
	C is A + B.

%--------------------

st01_build_score_list('_','_','_').
st01_build_score_list([],[],[]).
st01_build_score_list([SC|T1],[[SN|_]|T2],[[SC,SN]|T3]):-
	st01_build_score_list(T1,T2,T3).

%--------------------

st01_search_calc_hoods('_',_,'_').
st01_search_calc_hoods([],_,[]).
st01_search_calc_hoods([[_,L]|T],C,[S|ST]):-
	st01_calculate_overlap(L,C,O),
	st01_list_size(L,LS),
	S is O / LS,
	st01_search_calc_hoods(T,C,ST).

%--------------------

st01_list_size([],0).
st01_list_size([_|T],N):-
	st01_list_size(T,M),
	N is M + 1.

%--------------------

st01_calculate_overlap([],_,0).
st01_calculate_overlap([H|T], L, X):-
	exists(H,L),
	st01_calculate_overlap(T,L,Y),
	X is Y + 1.
st01_calculate_overlap([H|T], L, X):-
	not(exists(H,L)),
	st01_calculate_overlap(T,L,X).

%--------------------

st01_strip_conjoin_context_chl([_,[]],[]).
st01_strip_conjoin_context_chl([X,[H|T1]],T2):-
	st01_strip_conjoin_contexthoodlist(H,N),
	stick_on(T3,N,T2),
	st01_strip_conjoin_context_chl([X,T1],T3).
st01_strip_conjoin_context_chl([X,['_'|T1]],T2):-
	st01_strip_conjoin_context_chl([X,T1],T2).

%--------------------

st01_strip_conjoin_body_chl([],[]).
st01_strip_conjoin_body_chl([H|T1],T2):-
	st01_strip_conjoin_contexthoodlist(H,N),
	append(T3,N,T2),
	st01_strip_conjoin_body_chl(T1,T3).
st01_strip_conjoin_body_chl(['_'|T1],T2):-
	st01_strip_conjoin_body_chl(T1,T2).

%--------------------

st01_strip_conjoin_contexthoodlist([],[]).
st01_strip_conjoin_contexthoodlist([[_,L]|T],S):-
	append(L,P,S),
	st01_strip_conjoin_contexthoodlist(T,P).

%--------------------

st01_obtain_context_list(L,R):-
	st01_context_head(L,LH),
	st01_context_body(L,LB),
	st01_context_tail(L,LT),
	append([LH],LB,X),
	append(X,[LT],R).

st01_context_head([H,A,B|_],[H,C]):-
	append(A,B,C).

st01_context_body([_,_],[]).
st01_context_body([A,B,C|T],[[B,H]|T2]):-
	append(A,C,H),
	st01_context_body([B,C|T],T2).

st01_context_tail(L,LT):-
	reverse(L,R),
	st01_context_head(R,LT).

%--------------------


st01_obtain_refined_hood_list([],[]).
st01_obtain_refined_hood_list([H|T],[H1|T1]):-
	st01_create_refined_sentence_hood(H,H1),
	not(H1 == []),
	st01_obtain_refined_hood_list(T,T1).
st01_obtain_refined_hood_list([H|T],L):-
	st01_create_refined_sentence_hood(H,[]),
	st01_obtain_refined_hood_list(T,L).

st01_create_refined_sentence_hood([],[]).
st01_create_refined_sentence_hood([H|T],[H1|T1]):-
	not(H == '_'),
	st01_obtain_senses_hood_list(H,H1),
	st01_create_refined_sentence_hood(T,T1).
st01_create_refined_sentence_hood([H|T],[H|T1]):-
	H == '_',
	st01_create_refined_sentence_hood(T,T1).	
	
%--------------------

st01_obtain_senses_hood_list(W,L):-
	st01_obtain_senses(W,S),
	st01_create_senses_hood_list(S,L).

%--------------------

st01_create_senses_hood_list([],[]).
st01_create_senses_hood_list([H|T],[[H,X]|T1]):-
	st01_obtain_hood(H,X),
	st01_create_senses_hood_list(T,T1).

%--------------------

st01_obtain_senses(W,S):-
	findall(S,s(S,_,W,'n',_,_),S).

%--------------------

st01_obtain_hood(N,L):-
	st01_obtain_gloss(N,A),
	st01_obtain_hyponyms(N,B),
	st01_obtain_hypernyms(N,C),	
	st01_obtain_holonyms(N,D),
	st01_obtain_meronyms(N,E),
	append(A,B,X),
	append(X,C,Y),
	append(Y,D,Z),
	append(Z,E,L).
	
%--------------------

st01_obtain_gloss(N,D):-
	g(N,X),
	string_to_list(X,Y),
	st01_remove_non_char(Y,Z),
	st01_split_string(Z,A), !,
	st01_all_to_atom(A,B),
	st01_cut_word_list(B,C),
	clean_list(C,D).

%--------------------

st01_remove_non_char([],[]).
st01_remove_non_char([H|T],[H|T1]):-
	H > 96,
	H < 123,
	st01_remove_non_char(T,T1).

st01_remove_non_char([H|T],[H|T1]):-
	H > 64,
	H < 91,
	st01_remove_non_char(T,T1).

st01_remove_non_char([H|T],[H|T1]):-
	H = 32,
	st01_remove_non_char(T,T1).

st01_remove_non_char([H|T],T1):-
	H > 90,
	H < 97,
	st01_remove_non_char(T,T1).

st01_remove_non_char([H|T],T1):-

	H > 122,
	st01_remove_non_char(T,T1).
	
st01_remove_non_char([H|T],T1):-
	H < 65,

	not(H == 32),
	st01_remove_non_char(T,T1).	

%--------------------

st01_split_string([],[]).

st01_split_string(L,[H|T]):-
	st01_cut_out_word(L,H,M), 
	st01_split_string(M,T).

st01_cut_out_word([],[],[]).

st01_cut_out_word([H|T],[],T):- 
	H == 32.
	
st01_cut_out_word([H|T],[H|T2],L):- 
	not(H == 32),
	st01_cut_out_word(T,T2,L).

%--------------------

st01_all_to_atom([],[]).
st01_all_to_atom([H|T],[A|T1]):-
	string_to_list(W,H),
	string_to_atom(W,A),
	st01_all_to_atom(T,T1).

%--------------------

st01_obtain_hyponyms(N,L):-
	findall(X,hyp(X,N),X),
	st01_find_words(X,L1), !,
	remove_duplicates(L1,L).

%--------------------

st01_obtain_hypernyms(N,L):-
	findall(X,hyp(N,X),X),
	st01_find_words(X,L1), !,
	remove_duplicates(L1,L).

%--------------------

st01_obtain_holonyms(N,L):-
	findall(X,mm(N,X),X),
	st01_find_words(X,A), 
	findall(Y,ms(N,Y),Y),
	st01_find_words(Y,B), 
	findall(Z,mp(N,Z),Z),
	st01_find_words(Z,C), !,
	append(A,B,L1),
	append(L1,C,L2),
	remove_duplicates(L2,L).

%--------------------

st01_obtain_meronyms(N,L):-
	findall(X,mm(X,N),X),
	st01_find_words(X,A), 
	findall(Y,ms(Y,N),Y),
	st01_find_words(Y,B),
	findall(Z,mp(Z,N),Z),
	st01_find_words(Z,C), !,
	append(A,B,L1),
	append(L1,C,L2),
	remove_duplicates(L2,L).

%--------------------

st01_find_words([],[]).
st01_find_words([HH|HT],L):-
	st01_lookup_word(HH,A),
	append(PL,A,L), 
	st01_find_words(HT,PL).
	
st01_lookup_word(HH,X):-
	findall(X,s(HH,_,X,_,_,_),X).

%--------------------

st01_conjoin_raw_senses(X,Z):-
	st01_conjoin_sense_list(X,Y),
	st01_re_conjoin_list(Y,Z).

st01_re_conjoin_list([],[]).
st01_re_conjoin_list([H|T],M):-
	st01_re_conjoin_list(T,N),
	append(N,H,M).

st01_conjoin_sense_list([],[]).
st01_conjoin_sense_list([S|T1],L):-
	st01_conjoin_sense_list(T1,T2),
	st01_remove_ul(S,S1),
	append(T2,S1,L).

st01_remove_ul([],[]).
st01_remove_ul(['_'|T1],L):-
	st01_remove_ul(T1,L),!.
st01_remove_ul([H|T1],[H|T2]):-
	not(H == '_'),
	st01_remove_ul(T1,T2).

%----------------------------

st01_add_senselist(F,[],F).
st01_add_senselist(F,[H|T],P):-
	st01_add_sense(F,H,N),
	st01_add_senselist(N,T,P).

st01_add_sense([[X,N]|T1],[Y,R],L):-
	lc_equals(X,R),
	M is N + Y,
	append([[R,M]],T1,L).

st01_add_sense([[X,N]|T1],[Y,R],[[X,N]|T2]):-
	not(lc_equals(X,R)),
	st01_add_sense(T1,[Y,R],T2).

st01_add_sense([],[N,R],[[R,N]]).

%----------------------------

st01_remove_h_from_t('_',R,R).
st01_remove_h_from_t([],R,R).
st01_remove_h_from_t([H|T],B,R):-
	st01_remove_x(H,B,NB),
	st01_remove_h_from_t(T,NB,R).

st01_remove_x(_,[],[]).
st01_remove_x(X,[H|T1],[H|T2]):-
	not(X == H),
	st01_remove_x(X,T1,T2).
st01_remove_x(X,[H|T],B):-
	X == H,
	st01_remove_x(X,T,B).

%----------------------------

st01_rank_results('_','_').
st01_rank_results([],[]).
st01_rank_results(A,B):-
	st01_qsort(A,B).

st01_qsort([],[]).
st01_qsort([H|T], L2):-
	st01_extract_number(H,N),
	st01_partition(N,T,Littles,Bigs),
	st01_qsort(Littles,SLittles),
	st01_qsort(Bigs,SBigs),
	append(SBigs, [H|SLittles], L2).

st01_partition(_,[],[],[]).
st01_partition(O, [H|T], [H|LT], Bigs):-
	st01_extract_number(H,N),
	O >= N,
	st01_partition(O, T, LT, Bigs).
st01_partition(O, [H|T], Littles, [H|BT]):-
	st01_extract_number(H,N),
	O < N,
	st01_partition(O, T, Littles, BT).

st01_extract_number([H|_],H).

%--------------------