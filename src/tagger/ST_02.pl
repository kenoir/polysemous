:- module(run_search, [run_search/1]).

%--------------------

run_search(R):-
	st02_validate_program(R),
	st02_write_choices,
	st02_run_menu(R,'0').

%--------------------

st02_run_menu(R,Z):-
	st02_is_valid_choice(Z),
	st02_run_choice(R,Z),
	read(X),
	term_to_atom(X,Y),
	st02_run_menu(R,Y).
st02_run_menu(R,Z):-
	not(st02_is_valid_choice(Z)),
	write('\n'),
	write('\n'),
	write('That choice is invalid, please try again.'),
	write('\n'),
	write('\n'),
	term_to_atom(0,Y),
	st02_run_menu(R,Y).

st02_run_choice(_,N):-
	N == '0',
	write('\n'),
	write('Your choice: ').

st02_run_choice(R,N):-
	N == '1',
	write('\n'),
	st02_list_files(R,_),
	write('\n'),
	write('Listing Complete.'),
	write('\n'),
	write('Your choice: ').

st02_run_choice(R,N):-
	N == '2',
	st02_remove_file(R),
	write('\n'),
	write('Removal Complete.'),
	write('\n'),
	write('Your choice: ').

st02_run_choice(R,N):-
	N == '3',
	write('\n'),
	write('... Please Wait ...'),
	write('\n'),
	st02_obtain_valid_path_to_document_set(R),
	write('Tagging Complete.'),
	write('\n'),
	write('Your choice: ').

st02_run_choice(_,N):-
	N == '4',
	st02_write_choices,
	write('\n'),
	write('Your choice: ').

st02_run_choice(_,N):-
	N == '5',
	write('\n'),
	write('Program terminated.'),
	write('\n'),
	halt.

st02_write_choices:-
	write('Please choose from the following list of options:'),
	write('\n'),
	write('\n'),
	write('1: List files.'),
	write('\n'),
	write('2: Remove file from search set.'),
	write('\n'),
	write('3: Run tagging algorithm on untagged files.'),
	write('\n'),
	write('4: Relist options.'),
	write('\n'),
	write('5: Exit program').

st02_is_valid_choice(X):-
	atom_codes(X,C),
	st02_strip_list(C,D),
	D > 47,
	D < 54.

st02_strip_list([X],X).
	
%--------------------

st02_validate_program(R):-
	st02_validate_dir(R),
	st02_validate_sr(R),
	st02_validate_sm(R).

st02_validate_dir(R):-
	st02_validate_lib(R),
	st02_validate_tp(R),
	st02_validate_up(R).

st02_validate_sm(R):-
	append_atoms(R,'lib/sensemat.txt',SMP),
	exists_file(SMP).
st02_validate_sm(R):-
	append_atoms(R,'lib/sensemat.txt',SMP),
	not(exists_file(SMP)),
	append(SMP),
	told.

st02_validate_sr(R):-
	append_atoms(R,'lib/senseref.txt',SRP),
	exists_file(SRP).
st02_validate_sr(R):-
	append_atoms(R,'lib/senseref.txt',SRP),
	not(exists_file(SRP)),
	append(SRP),
	told.

st02_validate_lib(R):-
	append_atoms(R,'lib/',LP),
	exists_directory(LP).
st02_validate_lib(R):-
	append_atoms(R,'lib/',LP),
	not(exists_directory(LP)),
	make_directory(LP).

st02_validate_tp(R):-
	append_atoms(R,'lib/tdoc/',TP),
	exists_directory(TP).
st02_validate_tp(R):-
	append_atoms(R,'lib/tdoc/',TP),
	not(exists_directory(TP)),
	make_directory(TP).

st02_validate_up(R):-
	append_atoms(R,'lib/udoc/',UP),
	exists_directory(UP).
st02_validate_up(R):-
	append_atoms(R,'lib/udoc/',UP),
	not(exists_directory(UP)),
	make_directory(UP).

%--------------------

st02_list_files(R,NFL):-
	append_atoms(R,'lib/tdoc/*',TP),
	append_atoms(R,'lib/udoc/*',UP),
	expand_file_name(TP,TPL),
	expand_file_name(UP,UPL),
	st02_write_file_names(R,UPL,TPL,NFL,1).

st02_write_file_names(_,[],_,[],_).
st02_write_file_names(R,[UH|UT],TPL,[[N,UH]|T],N):-
	file_base_name(UH,UHN),
	append_atoms(R,'lib/tdoc/',BN),
	append_atoms(BN,UHN,THN),
	not(exists(THN,TPL)),
	write(N),
	write(': '),
	write(UHN),
	write(': Untagged'),
	write('\n'),
	M is N + 1,
	st02_write_file_names(R,UT,TPL,T,M).
st02_write_file_names(R,[UH|UT],TPL,[[N,UH]|T],N):-
	file_base_name(UH,UHN),
	append_atoms(R,'lib/tdoc/',BN),
	append_atoms(BN,UHN,THN),
	exists(THN,TPL),
	write(N),
	write(': '),	
	write(UHN),
	write(': Tagged'),
	write('\n'),
	M is N + 1,
	st02_write_file_names(R,UT,TPL,T,M).
	
%--------------------

st02_obtain_valid_path_to_document_set(R):-
	exists_directory(R),
	obtain_tag_document_set_members(R),
	create_sense_matrix(R).

st02_obtain_valid_path_to_document_set(R):-
	not(exists_directory(R)),
	write('Invalid Root Directory').

%--------------------

st02_remove_file(R):-
	st02_list_files(R,NFL),
	st02_process_remove_response(NFL,R).

st02_process_remove_response(NFL,R):-
	st02_is_single_element(NFL),
	write('\n'),
	write('Please enter a valid file number for removal: '),
	read(X),
	st02_extract_filename(X,NFL,FP),
	file_base_name(FP,P),
	append_atoms(R,'lib/udoc/',UBN),
	append_atoms(UBN,P,UFN),
	append_atoms(R,'lib/tdoc/',TBN),
	append_atoms(TBN,P,TFN),
	st02_remove_udoc(UFN),
	st02_remove_tdoc(TFN),
	st02_empty_sref(R),
	st02_empty_smat(R).


st02_process_remove_response(NFL,R):-
	not(st02_is_single_element(NFL)),
	write('\n'),
	write('Please enter a valid file number for removal: '),
	read(X),
	st02_extract_filename(X,NFL,FP),
	file_base_name(FP,P),
	append_atoms(R,'lib/udoc/',UBN),
	append_atoms(UBN,P,UFN),
	append_atoms(R,'lib/tdoc/',TBN),
	append_atoms(TBN,P,TFN),
	st02_remove_sref(TFN,R),
	st02_remove_udoc(UFN),
	st02_remove_tdoc(TFN),
	create_sense_matrix(R).

%------------

st02_is_single_element([[_,_]]).

%------------

st02_extract_filename(_,[],'fail').
st02_extract_filename(N,[[N,P]|_],P).
st02_extract_filename(N,[[M,_]|T],X):-
	not(N == M),
	st02_extract_filename(N,T,X).

st02_remove_udoc(FN):-
	not(exists_file(FN)),
	write('\n'),
	write('File does not exist.'),
	write('\n').

st02_remove_udoc(FN):-
	exists_file(FN),
	delete_file(FN).

%------------

st02_remove_sref(TFN,_):-
	not(exists_file(TFN)).

st02_remove_sref(TFN,R):-
	exists_file(TFN),
	obtain_document_senses(TFN,DSL,_),
	append_atoms(R,'lib/senseref.txt',S),
	read_senseref(S,SL),
	st02_remove_senselist(SL,DSL,NSL),
	write_senseref(S,NSL).

%------------

st02_empty_sref(R):-
	append_atoms(R,'lib/senseref.txt',S),
	tell(S),
	told.

%------------

st02_empty_smat(R):-
	append_atoms(R,'lib/sensemat.txt',S),
	tell(S),
	told.

%------------

st02_remove_tdoc(FN):-
	not(exists_file(FN)).

st02_remove_tdoc(FN):-
	exists_file(FN),
	delete_file(FN).

%----------------------------

st02_remove_senselist(F,[],F).
st02_remove_senselist(F,[H|T],P):-
	st02_remove_sense(F,H,N),
	st02_remove_senselist(N,T,P).

st02_remove_sense([[R,N]|T1],R,L):-
	M is N - 1,
	not(M == 0),
	append([[R,M]],T1,L).

st02_remove_sense([[R,N]|T1],R,T1):-
	M is N - 1,
	M == 0.

st02_remove_sense([[X,N]|T1],R,[[X,N]|T2]):-
	not(X == R),
	st02_remove_sense(T1,R,T2).

st02_remove_sense([],_,[]).

%----------------------------