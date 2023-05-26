% Call-by-value lambda-calculus, big-step semantics
% Fig 2, Vesely and Fisher 2019

bigstep(val(V),_,V).
bigstep(var(X),Rho,V) :-
	find(Rho,X,V).
bigstep(lam(X,E),Rho,clo(X,E,Rho)).
bigstep(app(E1,E2),Rho,V) :-
	bigstep(E1,Rho,clo(X,E,Rho1)),
	bigstep(app2(clo(X,E,Rho1),E2),Rho,V).
bigstep(app2(clo(X,E,Rho1),E2),Rho,V) :-
	bigstep(E2,Rho,V2),
	save(X,V2,Rho1,Rho2),
	bigstep(E,Rho2,V).

/*
bigstep(app(E1,E2),Rho,V) :-
	bigstep(E1,Rho,clo(X,E,Rho1)),
	bigstep(E2,Rho,V2),
	save(X,V2,Rho1,Rho2),
	bigstep(E,Rho2,V).
*/

find([(X,N)|_],X,N).
find([(Y,_)|St],X,N) :-
	X \== Y,
	find(St,X,N).
	
save(X,V,[(X,_)|St],[(X,W)|St]) :-
	W = V.
save(X,V,[(Y,M)|St],[(Y,M)|St1]) :-
	X \== Y,
	save(X,V,St,St1).
save(X,V,[],[(X,W)]) :-
	W = V.
	
leaf(bigstep(var(_),_,_)).
leaf(bigstep(val(_),_,_)).
leaf(bigstep(lam(_,_),_,_)).

nonLeaf(bigstep(app(_,_),_,_)).
nonLeaf(bigstep(app2(_,_),_,_)).

