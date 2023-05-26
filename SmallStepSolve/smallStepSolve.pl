:- module(smallStepSolve,_).

run([]).
run([A]) :-
	bigStepPred(A),
	smallStep(A,As1),
	run(As1).

smallStep(A,[]) :-
	leaf(A),
	clpClause(_,A,Bs),
	callPreds(Bs).
smallStep(bigstep(A,St0,St1),As1) :-
	nonLeaf(bigstep(A,St0,St1)),
	clpClause(K,bigstep(A,St0,St1),[B|Bs]),
	evalConditions([B|Bs],[B1|Bs1]),
	nextStep([B1|Bs1],K,As1).
	
nextStep([A|Bs],_,Bs1) :-
	leaf(A),
	evalConditions(Bs,Bs1),
	smallStep(A,[]).
nextStep([bigstep(A,St0,St1)|Bs],K,[H]) :-
	nonLeaf(bigstep(A,St0,St1)),
	smallStep(bigstep(A,St0,St1),[bigstep(U,V,W)]),
	tryFold(K,H,[bigstep(U,V,W)|Bs]).
	
tryFold(_,B1,[B1]).
tryFold(K,H,[B1|Bs]) :-
	Bs \== [],
	clpClause(K,H,[B1|Bs]).
	
evalConditions([],[]).
evalConditions([B|Bs],[B|Bs]) :-
	bigStepPred(B).
evalConditions([B|Bs],Bs1) :-
	otherPred(B),
	eval(B),
	evalConditions(Bs,Bs1).

eval(B) :-
	constraint(B),
	call(B).
eval(B) :-
	clpClause(_,B,B1),
	callPreds(B1).
	
callPreds([]).
callPreds([B|Bs]) :-
	eval(B),
	callPreds(Bs).
	
bigStepPred(bigstep(_,_,_)).
bigStepPred(controlExpr(_,_,_,_)).
	
otherPred(B) :-
	functor(B,P,N),
	member(P/N,[eval/4,find/3,gt/3,lt/3,gte/3,lte/3,eq/3,negate/2,save/4]).
otherPred(B) :-
	constraint(B).
	
leaf(A) :-
	clpClause(_,leaf(A),[]).

nonLeaf(A) :-
	clpClause(_,nonLeaf(A),[]).

constraint(_ = _).
constraint(_ < _).
constraint(_ > _).
constraint(_ =< _).
constraint(_ >= _).
constraint(_ is _).
constraint(_ =:= _).
constraint(_\==_).
constraint(_\=_).


%% big step semantics for While

/*
clpClause(0,eval(var(A),B,B,C),[find(B,A,C)]).
clpClause(1,eval(cns(nat(A)),B,B,A),[]).
clpClause(2,eval(add(A,B),C,D,E),[eval(A,C,F,G),eval(B,F,D,H),E=G+H]).
clpClause(3,eval(sub(A,B),C,D,E),[eval(A,C,F,G),eval(B,F,D,H),E=G-H]).
clpClause(4,eval(mul(A,B),C,D,E),[eval(A,C,F,G),eval(B,F,D,H),E=G*H]).
clpClause(5,eval(div(A,B),C,D,E),[eval(A,C,F,G),eval(B,F,D,H),E=G/H]).
clpClause(6,controlExpr(A>B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),gt(G,H,E)]).
clpClause(7,controlExpr(A<B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),lt(G,H,E)]).
clpClause(8,controlExpr(A>=B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),gte(G,H,E)]).
clpClause(9,controlExpr(A=<B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),lte(G,H,E)]).
clpClause(10,controlExpr(A==B,C,D,E),[eval(A,C,F,G),eval(B,F,D,H),eq(G,H,E)]).
clpClause(11,controlExpr(logicaland(A,B),C,D,E),[eval(A,C,F,G),eval(B,F,D,H),E is G/\H]).
clpClause(12,controlExpr(not(A),B,C,D),[eval(A,B,C,E),negate(E,D)]).
clpClause(13,bigstep(skip,A,A),[]).
clpClause(14,bigstep(asg(var(A),B),C,D),[eval(B,C,E,F),save(A,F,E,D)]).
clpClause(15,bigstep(seq(A,B),C,D),[bigstep(A,C,E),bigstep(B,E,D)]).
clpClause(16,bigstep(ifthenelse(A,B,C),D,E),[controlExpr(A,D,F,1),bigstep(B,F,E)]).
clpClause(17,bigstep(ifthenelse(A,B,C),D,E),[controlExpr(A,D,F,0),bigstep(C,F,E)]).
clpClause(18,bigstep(while(A,B),C,D),[bigstep(ifthenelse(A,seq(B,while(A,B)),skip),C,D)]).
clpClause(19,bigstep(for(A,B,C,D),E,F),[bigstep(A,E,G),bigstep(while(B,seq(D,C)),G,F)]).
clpClause(20,find([(A,B)|C],A,B),[]).
clpClause(21,find([(A,B)|C],D,E),[D\==A,find(C,D,E)]).
clpClause(22,save(A,B,[(A,C)|D],[(A,E)|D]),[E is B]).
clpClause(23,save(A,B,[(C,D)|E],[(C,D)|F]),[A\==C,save(A,B,E,F)]).
clpClause(24,save(A,B,[],[(A,C)]),[C is B]).
clpClause(25,gt(A,B,1),[A>B]).
clpClause(26,gt(A,B,0),[A=<B]).
clpClause(27,lt(A,B,1),[A<B]).
clpClause(28,lt(A,B,0),[A>=B]).
clpClause(29,gte(A,B,1),[A>=B]).
clpClause(30,gte(A,B,0),[A<B]).
clpClause(31,lte(A,B,1),[A=<B]).
clpClause(32,lte(A,B,0),[A>B]).
clpClause(33,eq(A,B,1),[A==B]).
clpClause(34,eq(A,B,0),[A\==B]).
clpClause(35,negate(true,false),[]).
clpClause(36,negate(false,true),[]).
clpClause(37,leaf(bigstep(skip,A,B)),[]).
clpClause(38,leaf(bigstep(asg(A,B),C,D)),[]).
clpClause(39,leaf(controlExpr(A,B,C,D)),[]).
clpClause(40,nonLeaf(bigstep(seq(A,B),C,D)),[]).
clpClause(41,nonLeaf(bigstep(ifthenelse(A,B,C),D,E)),[]).
clpClause(42,nonLeaf(bigstep(while(A,B),C,D)),[]).
clpClause(43,nonLeaf(bigstep(for(A,B,C,D),E,F)),[]).
*/

%% big step semantics for Lambda

clpClause(0,bigstep(val(A),B,A),[]).
clpClause(1,bigstep(var(A),B,C),[find(B,A,C)]).
clpClause(2,bigstep(lam(A,B),C,clo(A,B,C)),[]).
clpClause(3,bigstep(app(A,B),C,D),[bigstep(A,C,clo(E,F,G)),bigstep(app2(clo(E,F,G),B),C,D)]).
clpClause(4,bigstep(app2(clo(A,B,C),D),E,F),[bigstep(D,E,G),save(A,G,C,H),bigstep(B,H,F)]).
clpClause(5,find([(A,B)|C],A,B),[]).
clpClause(6,find([(A,B)|C],D,E),[D\==A,find(C,D,E)]).
clpClause(7,save(A,B,[(A,C)|D],[(A,E)|D]),[E=B]).
clpClause(8,save(A,B,[(C,D)|E],[(C,D)|F]),[A\==C,save(A,B,E,F)]).
clpClause(9,save(A,B,[],[(A,C)]),[C=B]).
clpClause(10,leaf(bigstep(var(A),B,C)),[]).
clpClause(11,leaf(bigstep(val(A),B,C)),[]).
clpClause(12,leaf(bigstep(lam(A,B),C,D)),[]).
clpClause(13,nonLeaf(bigstep(app(A,B),C,D)),[]).

test(S) :-
	exCode(C),
	run([bigstep(C,[(x,5),(y,2),(z,3)],S)]).
	
%exCode(seq(seq(asg(var(x),add(var(z),var(y))),asg(var(z),var(x))),asg(var(y),cns(nat(7))))).
exCode(while(var(x)>cns(nat(0)),asg(var(x),sub(var(x),cns(nat(1)))))).


