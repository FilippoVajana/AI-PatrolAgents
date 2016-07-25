/******************   MODULO PER LA VALUTAZIONE DI ESPRESSIONI GENERICHE *****
******************************************************************************/
:-module(eval, [eval/2, eval/3]).

%%	eval(+Ass, +Expr, -Val)
%       1) Expr è una espressione ground con possibili variabili
%	Ad es. x+2*y con variabili x, y
%	dove però interpretiano + come concatenazione di liste e n*L
%	come moltiplicazione L+L+...+L n volte
%	(NB: x,y sono variabili nella espressione, NON per Prolog)
%
%       2) Ass è un assegnamento di valori alle variabili, ad es.
%	[x=[a,b],y=[b,a]]
%
%       3) Val è il valore di Expr con assegnamento Ass,
%	nel nostro esempio: val([x=[a,b],y=[b,a]], x+2*y) =
%	[a,b]+2*[b,a] = [a,b]+2*[b,a] = [a,b,b,a,b,a]
%
eval(Ass, Expr, Val) :-
	( value(Expr,Val); member(Expr=Val,Ass)),
	!.
eval(Ass, Expr, Val) :-
	Expr =.. [Op|Args],
	maplist(eval(Ass), Args, ValArgs),
	apply(Op, ValArgs, Val).

%%	Valutazione di espressione priva di variabili
eval(Expr,Val) :-
	eval([],Expr,Val).


