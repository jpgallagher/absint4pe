:- module(prop2logen,[main/1]).

:- use_module(library(lists)).

main([InF,OutF]) :-
	open(InF,read,S1),
	open(OutF,write,S2),
	translateFile(S1,S2),
	close(S1),
	close(S2).
main([InF]) :-
	open(InF,read,S1),
	translateFile(S1,user_output),
	close(S1).	
	

translateFile(S1,S2) :-
	read(S1,C),
	(
	    C == end_of_file -> true
	;
	    numbervars(C,0,_),
		writePropClause(C,S2),
	    translateFile(S1,S2)
	).

writePropClause((A :- B),S) :-
	!,
	writeq(S,logen(prop/2,prop(A,B))),
	write(S,'.'),
	nl(S).
writePropClause((:- _),_).

