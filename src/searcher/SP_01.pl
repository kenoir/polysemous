:- module(obtain_query_terms, [obtain_query_terms/1]).

%--------------------

obtain_query_terms(L):-
	write('Please enter a search term, (or enter x to end):'),
	read(W),
	sp01_where_next(W,L), !.

%--------------------

sp01_where_next(W,[]):-
	W == 'x'.

sp01_where_next(W,T):-
	not(W == 'x'),
	not(sp01_is_word_valid(W)),
	stem(W,S),
	not(sp01_is_word_valid(S)),
	write('\n'),
	write('This word does not appear in the dictionary, please try again.'),
	write('\n'),
	obtain_query_terms(T).

sp01_where_next(W,[N|T]):-
	not(W == 'x'),
	not(sp01_is_word_valid(W)),
	stem(W,S),
	sp01_is_word_valid(S),
	write('\n'),
	write('Please select a number from the following list that most closely matches your intended meaning.'),	
	write('\n'),	
	sp01_obtain_senses(S,_),
	write('Your choice: '),
	sp01_validate_choice(S,C),
	s(N,_,S,'n',C,_),	
	obtain_query_terms(T).

sp01_where_next(W,[N|T]):-
	not(W == 'x'),
	sp01_is_word_valid(W),
	write('\n'),
	write('Please select a number from the following list that most closely matches your intended meaning.'),	
	write('\n'),	
	sp01_obtain_senses(W,_),
	write('Your choice: '),
	sp01_validate_choice(W,C),
	s(N,_,W,'n',C,_),	
	obtain_query_terms(T).

%--------------------

sp01_validate_choice(S,C):-
	read(B),
	sp01_validate_choices(S,B,C).

sp01_validate_choices(S,C,C):-
	s(_,_,S,'n',C,_).	

sp01_validate_choices(S,B,C):-
	not(s(_,_,S,'n',B,_)),
	write('\n'),
	write('That choice is invalid, please try again: '),
	sp01_validate_choice(S,C).

%--------------------
	
sp01_obtain_senses(W,L):-
	sp01_write_sense(L,1,W).

sp01_write_sense([],N,W):-
	not(s(_,_,W,'n',N,_)).

sp01_write_sense([H|T],N,W):-
	s(H,_,W,'n',N,_),
	g(H,G),
	write(N),
	write(': '),
	write(G),
	write('\n'),
	M is N + 1,
	sp01_write_sense(T,M,W).

%--------------------

sp01_is_word_valid(W):-
	s(_,_,W,'n',_,_).