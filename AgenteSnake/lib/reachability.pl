:- module(reachability, [path/2,
				 reachable/4
				]).

:- use_module('ptc/is_a').
type_check :- type_check_module(reachability).

/*****    PERCORSI IN UN GRAFO,  CODICE GUIDA ****/

type [[], [Type|list(Type)]]:list(Type)
     :"liste con elementi di tipo generico".
%  usata per rappresentare i cammini
type pred(_,_)
     :"meta-tipo dei predicati binari".
%  usato per il parametro Arc che definisce il grafo

pred path(list(Nodo), pred(Nodo,Nodo)).
%   path(--L, +Arc) nondet:
%   PRECONDIZIONE:  il grafo rappresentato da Arc(X,Y) � aciclico
%   Spec: L � un cammino del grafo rappresentato da Arc;
%   NOTA.  Se la precondizione non � vera si hanno infinite risposte
%   e l'insieme delle risposte pu� essere incompleto

pred reachable(Nodo, Nodo, list(Nodo), pred(Nodo,Nodo)).
%   reachable(+N1, ?N2, ?L, +Arc) nondet: [N1|L] � un cammino del grafo
%   rappresentato da Arc che inizia con N1 e termina con N2
%   e L � aciclico e non passa per Visited

pred anello(Nodo, list(Nodo), pred(Nodo,Nodo)).
%   anello(?N, ?L) nondet.
%   Spec.   [N|L] � un anello che inizia e termina in N

/****  IMPLEMENTAZIONE  *****/
%%	predicato ausiliario:
pred reachable(Nodo, Nodo, list(Nodo), list(Nodo), pred(Nodo,Nodo)).
%   reachable(+N1,?N2,?L,++Visited)  nondet
%   Spec:  [N1|L] � un cammino aciclico da N1 a N2 che non
%   contiene nodi in Visited
%   Ad ogni chiamata ricorsiva faccio in modi che Visited
%   contenga i nodi gi� visitati; deve essere ground
%   per evitare il floundering

%%	CODICE

%  copertura [],[X], [X,Y|L]
path([],_).
path([_], _).
path([X,Y|L], Arc) :-
	call(Arc,X,Y),
	path([Y|L], Arc).

reachable(X,Y,L, Arc) :-
	reachable(X,Y,L,[], Arc).

reachable(X,X,[],_Visited,_Arc).
% Base: Spec(reachable(X,X,[],Visited):
%         [X|[]] � un cammino del grafo
%         e [] � aciclico e non passa per Visited
reachable(X,Y,[Z|L],Visited,Arc) :-
	call(Arc,X,Z),
	not(member(Z,Visited)),
	reachable(Z,Y,L,[Z|Visited],Arc).
% Passo: assumo Spec(reachable(Z,Y,L,[Z|Visited],Arc):
%         (1)  [X|L] � un cammino del grafo da Z a Y e
%         (2)  L � aciclico e non passa per [Z|Visited];
%         ho (3) not(member(Z,Visited))
%	     (4) arc(X,Z)
%         ottengo  Spec(reachable(X,Y,[Z|L],Visited,Arc);
%         infatti:
%         (1')  [X,Z|L] � un cammino da X a Y per (1) e (4)
%         (2')  [Z|L] � aciclico per (2) e non passa per
%		Visited per (2) e (3)

