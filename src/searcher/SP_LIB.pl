%--------------------

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

is_end_tag(X):-
	X == 62.

extract_number([H|_],H).
extract_value([_,A|_],A).

number_occurences([],_,1).
number_occurences([H|T],A,X):-
	H == A,
	number_occurences(T,A,Y),
	X is Y + 1.
number_occurences([H|T],A,N):-
	not(H == A),
	number_occurences(T,A,N).

append_atoms(X,Y,Z):-
	string_to_atom(SX,X),
	string_to_list(SX,LX),
	string_to_atom(SY,Y),
	string_to_list(SY,LY),
	append(LX,LY,LZ),
	string_to_list(SZ,LZ),
	string_to_atom(SZ,Z).

%--------------------