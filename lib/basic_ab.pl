/*******  ASSUMPTION BASED REASONING: modulo di base ******
*         uso_basic_ab.pl mostra come usarlo              *
***********************************************************/

:- module(basic_ab, [op(100, xfx, (::)),
		     prove/3,
		     prove/4,
		     (::)/2]).

%%	prove(+Body, -Assumptions)  nondet
%       Assumptions  U  KB_user   |=  Body[X1=t1,.., Xn=tn]
%       dove [X1=t1,.., Xn=tn] è la sostituzione di risposta
prove(Body, Assumptions, Pt) :-
      prove(Body, [], Assumptions, Pt).


/****************  BODY NON ATOMICI  *************/

prove((Body1, Body2),Ass1, Ass2, (Pt1,Pt2)) :- !,
	prove(Body1, Ass1,Ass, Pt1),
	prove(Body2, Ass, Ass2, Pt2).
prove((Body1; Body2),Ass1, Ass2, Pt) :- !,
	(   prove(Body1,Ass1, Ass2,Pt);
	    prove(Body2, Ass1, Ass2, Pt)  ).

/***********  GOAL  **************/

prove(A, Ass,Ass, true) :-
	prolog_goal(A), !.

prove(A, Ass1, Ass2, Pt) :-
	(schema(A,SA), meta_clause(_,SA,_); user:assumable(A)),!,
	% se c'è una metaclausola con testa A oppure A è un assumibile,
	% metainterpreto A
	prove_meta_goal(A, Ass1, Ass2, Pt).


/************  Se non c'è una metaclausola con testa A e A non è un assumibile
 LO TRATTO COME GOAL PROLOG, WARNING+fail IN CASO SIA INDEFINITO  ***********/

prove(A, Ass, Ass, pt(A, called)) :-
	catch(call(A), E, (writeln('WARNING UNDEDFINED ':E), fail)).


/*************** METAINTERPRETAZIONE DI UN METAGOAL *****/

prove_meta_goal(A, Ass1, Ass2, Pt) :-
	meta_clause(N,A,Body),
	prove(Body,Ass1,Ass2, PtBody),
	Pt = pt(N::A, PtBody)
	;
        user:assumable(A),
	user:update_ass(A, Ass1, Ass2),
	Pt = pt(A, assumed).



/****************************** UTIL ***********************/

_::_.

meta_clause(N, A, true) :-
	user:clause(A, meta::N).
meta_clause(N, A, Body) :-
	user:clause(A, (meta::N, Body)).

schema(A,SA) :-
	functor(A,F,N),
	functor(SA,F,N).


prolog_goal(is_a(_X,_Type)).
prolog_goal((_X = _Y)).
prolog_goal((_X \= _Y)).
prolog_goal((_X == _Y)).
prolog_goal((_X \== _Y)).
prolog_goal((_X @< _Y)).
prolog_goal((_X @> _Y)).
prolog_goal((_X @>= _Y)).
prolog_goal((_X @=< _Y)).
prolog_goal(copy_term(_X,_Y)).
prolog_goal((_X > _Y)).
prolog_goal((_X < _Y)).
prolog_goal((_X >= _Y)).
prolog_goal((_X =< _Y)).
prolog_goal((_X =:= _Y)).
prolog_goal((_X =\= _Y)).
prolog_goal(number(_I)).

prolog_goal(true).
prolog_goal(fail).
prolog_goal(!).
prolog_goal(abort).
prolog_goal(_V is _E).


prolog_goal(throw(_Err)).
prolog_goal(catch(_Goal,_,_Handler)).
prolog_goal(not(_Goal)).
prolog_goal(assert(_Goal)).
prolog_goal(retract(_Goal)).
prolog_goal(retractall(_Goal)).
prolog_goal(::(_,_)).












