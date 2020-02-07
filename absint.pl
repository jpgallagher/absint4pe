:- module(absint,_).

% property-based interpreter

:- use_module(chclibs(yices2_sat)).
:- use_module(chclibs(canonical)).
:- use_module(library(terms_vars)).
:- use_module(chclibs(linearize)).
:- use_module(chclibs(program_loader)).
:- use_module(ciao_yices(ciao_yices_2)).
:- use_module(library(read_from_string), [read_from_atom/2]).



/*
hornClause(start,[A=5,B=6,C=10],[while0(A,B,C)]).
hornClause(while0(A,B,C),[A>0],[if0(A,B,C)]).
hornClause(while0(A,B,C),[A=<0],[]).
hornClause(if0(A,B,C),[B<C,D=B+1],[while0(A,D,C)]).
hornClause(if0(A,B,C),[B>=C,D=A-1],[while0(D,B,C)]).
prop(if0(A,B,C),[-1*B+1*C>0]).
prop(if0(A,B,C),[1*A>0]).
prop(if0(A,B,C),[1*B+ -1*C>=0]).
prop(while0(A,B,C),[1*A>0]).
prop(while0(A,B,C),[-1*B+1*C>0]).
prop(while0(A,B,C),[1*B+ -1*C>=0]).
prop(while0(A,B,C),[-1*A>=0]).
prop(start,[]).
*/


go(G) :-
	read_from_atom(G,Goal),
	numbervars(Goal,0,_),
	instanceOf(Goal,A),
	%functor(Goal,P,N),
	program(Prog),
	proplist(Psi),
	yices_init,
	% initial abstract state = []
	solve(A,[],Psi,Prog).  	
	%yices_exit.
	
% solve(P/N,A,Phi,Psi,Prog):  P/N a predicate, A concrete, Phi abstract, Psi props, Prog clauses 
solve(A,Phi,Psi,Prog) :-
	member(hornClause(H,Theta,B),Prog),
	instanceOf(hornClause(H,Theta,B),hornClause(A,Theta1,B1)),
	delta_A(A,Theta1,B1,Phi,BPhi,Psi),
	delta(Theta1),
	%predCalls(B,PNs),
	solveConj(B1,BPhi,Psi,Prog).

solveConj([],[],_,_).
solveConj([B|Bs],[Phi|BPhi],Psi,Prog) :-
	solve(B,Phi,Psi,Prog),
	solveConj(Bs,BPhi,Psi,Prog).
	
delta([]).
delta([C|Cs]) :-
	call(C),
	delta(Cs).
	
%--------------
%
% Properties is set {(P/N,Ps) | P/N is a predicate, Ps is a set of properties on P/N}
% 
	
delta_A(H,Theta,B,Phi,BPhi,Psi) :-
	currentProps(H,Phi,Psi,H0,Cs),
	melt((H0,Cs),(H,Cs1)),
	append(Theta,Cs1,Ds),
	checkSat(Ds),
	bodyVersions(B,Ds,Psi,BPhi).
	
argEqs(H,A,Theta,Theta1) :-
	H =.. [F|Xs],
	A =.. [F|Ys],
	eqList(Xs,Ys,Theta1,Theta).
	
eqList([],[],Theta,Theta).
eqList([X|Xs],[Y|Ys],[X=Y|Theta1],Theta) :-
	eqList(Xs,Ys,Theta1,Theta).
	
currentProps(H,Phi,Psi,H0,Cs) :-
	functor(H,P,N),
	getPredProps(P/N,Psi,Props),
	selectIds(Phi,Props,PCs),
	intersectionConstraints(PCs,Cs),
	functor(H0,P,N),
	numbervars(H0,0,_).

getPredProps(P/N,Psi,Props) :-
	member(pred(P/N,Props),Psi).
getPredProps(P/N,Psi,[]) :-
	\+ member(pred(P/N,_),Psi).
	
bodyVersions([],_,_,[]).
bodyVersions([B|Bs],Ds,Psi,[Ids|BPhi]) :-
	abstractVersion(B,Ds,Psi,Ids),
	bodyVersions(Bs,Ds,Psi,BPhi).
		
abstractVersion(B,Ds,Psi,Ids) :-
	copy_term((B,Ds),(B1,Ds1)),
	varset(B1,Xs),
	numbervars((Xs,Ds1),0,_),
	predicate_abstract(B1,Ds1,Psi,Ids).
	
predicate_abstract(Head,F,L,Ids) :-
	functor(Head,P,N),
	predAbstract(P/N,F,L,Ids).

predAbstract(P/N,F,Props,Ids) :-
	member(pred(P/N,LPs),Props),
	!,
	selectProps(LPs,F,Ids).
predAbstract(_,_,_,[]).

selectProps([],_,[]).
selectProps([C1-Id|LPs],F,[Id|Ids]) :-
	entails(F,C1),
	!,
	selectProps(LPs,F,Ids).
selectProps([_|LPs],F,Ids) :-
	selectProps(LPs,F,Ids).	
	
checkSat(Cs) :-
	copy_term(Cs,MCs),
	varset(MCs,Vs),
	linearConstraints(MCs,LCs,_),
	numbervars(Vs,0,_),
	yices_vars(Vs,real,Ws),
	yices_sat(LCs,Ws).
	
entails(C1,C2) :-
	melt([C1,neg(C2)],E),
	varset(E,Vs),
	numbervars(Vs,0,_),
	yices_vars(Vs,real,Ws),
	yices_unsat(E,Ws).
	
selectIds([Id|Ids],[C-Id|Hs],[C|Hs1]) :-
	!,
	selectIds(Ids,Hs,Hs1).
selectIds([neg(Id)|Ids],[C-Id|Hs],[neg(C)|Hs1]) :-
	!,
	selectIds(Ids,Hs,Hs1).
selectIds([Id|Ids],[_-Id1|Hs],Hs1) :-
	Id @> Id1,
	!,
	selectIds([Id|Ids],Hs,Hs1).
selectIds([neg(Id)|Ids],[_-Id1|Hs],Hs1) :-
	Id @> Id1,
	!,
	selectIds([neg(Id)|Ids],Hs,Hs1).
selectIds([_|Ids],Hs,Hs1) :-
	selectIds(Ids,Hs,Hs1).
selectIds([],_,[]).

intersectionConstraints([],[]).
intersectionConstraints([neg(C)|Cs],[neg(C)|Ds]) :-
	!,
	intersectionConstraints(Cs,Ds).
intersectionConstraints([C|Cs],Ds) :-
	intersectionConstraints(Cs,Ds1),
	append(C,Ds1,Ds).
	
linearConstraints([],[],[]).
linearConstraints([neg(C)|Cs],[neg(C1)|LCs],[neg(Cs)|NLCs]) :-
	!,
	linearConstraints(C,C1,_),
	linearConstraints(Cs,LCs,NLCs).
linearConstraints([C|Cs],[C|LCs],NLCs) :-
	linear_constraint(C),
	!,
	linearConstraints(Cs,LCs,NLCs).
linearConstraints([C|Cs],LCs,[C|NLCs]) :-
	linearConstraints(Cs,LCs,NLCs).
	
	
program(P) :-
	findall((hornClause(H,Theta,B)),(
		hornClause(H,Theta,B),
		numbervars((H,Theta,B),0,K)),
	P).
	
predCalls([],[]).
predCalls([B|Bs],[P/N|PNs]) :-
	functor(B,P,N),
	predCalls(Bs,PNs).
	
proplist(L) :-
	proplist_k(L,_).
	
proplist_k(L,K) :-
	findall((A,C),(
		prop(A,C),
		numbervars((A,C),0,_)),
	Ps),
	makePropList(Ps,0,K,[],L).
	
makePropList([],K,K,L,L).
makePropList([(A,C)|Ps],K0,K2,L0,L2) :-
	functor(A,P,N),
	N>0,
	!,
	addProperty(P/N,C,K0,K1,L0,L1),
	makePropList(Ps,K1,K2,L1,L2).
makePropList([_|Ps],K0,K1,L0,L1) :-
	makePropList(Ps,K0,K1,L0,L1).
	
addProperty(P/N,C,K0,K1,[pred(P/N,Props)|Ps0],[pred(P/N,Props1)|Ps0]) :-
	!,
	addPredProps(Props,C,K0,K1,Props1).
addProperty(P/N,C,K0,K1,[PredProps|Ps0],[PredProps|Ps1]) :-
	addProperty(P/N,C,K0,K1,Ps0,Ps1).
addProperty(P/N,C,K0,K1,[],[pred(P/N,[C-K0])]) :-
	K1 is K0+1.

addPredProps([],C,K0,K1,[C-K0]) :-
	K1 is K0+1.
addPredProps([C-Id|Props],Cp,K0,K1,[C-Id|Props1]) :-
	addPredProps(Props,Cp,K0,K1,Props1).
	
% instance of a term in the ground representation
instanceOf(X,Y) :-
	melt(X,Y).