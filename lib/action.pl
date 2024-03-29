:- use_module(is_a).
:- use_module(action_spec).
:- use_module(mr).
:- use_module(action_help, [action_help/0]).

ignored(solve(_, _)).
ignored(last_solution(_,_,_)).

:- writeln(
'************* MODULO ACTION ********************
     default_strategy.
     solution(-Piano, -Costo).
     exec.
---> action_help.  per help

************************************************').

type T :- action_spec:type(T).
pred P :- action_spec:pred(P).

transition(Action, State, NewState, Cost) :-
	add_del(Action,State,Add, Del, Cost),
	list_to_ord_set(Add, OrdAdd),
	list_to_ord_set(Del, OrdDel),
	ord_subtract(State, OrdDel,St),
	ord_union(St, OrdAdd, NewState).

transition(S1,S2) :-
	transition(_,S1,S2,_).

:- dynamic(last_solution/3).
%    usato localmente come memorizzazione temporanea per l'eventuale
%    esecuzione successiva
solution(Sol, Cost) :-
	starting_state(S0),
	solve(S0,nc(S, Path, Cost)),
	reverse([S|Path], SL),
	states_to_actions(SL, Sol),
	retractall(last_solution(_,_,_)),
	assert(last_solution(S0, SL, Sol)),
	writeln('\nSe vuoi vedere l''esecuzione di questo piano, usa exec.').

default_strategy :-
	strategy(astar),
        strategy(pota_chiusi).

vicini(S,L) :-
	setof(S1, transition(S,S1), L),! ; L=[].
costo(S1,S2,C) :-
	transition(_,S1,S2,C).

states_to_actions([],[]).
states_to_actions([_S],[]).
states_to_actions([S1,S2|SL],[A|AL]) :-
	transition(A,S1,S2,_),
	states_to_actions([S2|SL], AL).

exec :-	last_solution(S0, _SL, Sol), !,
        exec(S0, Sol).
exec :- writeln('NESSUNA SOLUZIONE MEMORIZZATA'),
	fail.
exec(_,[]) :-
	writeln('END').
exec(S0, [A|P]) :-
	transition(A, S0, S1, _),
	writeln('AZIONE ':A),
	writeln('   DA ':S0),
	writeln('   A  ':S1),
	exec(S1,P).

mostra_nodo(nc(S, Path, Cost)) :-
	maplist(write, ['cost:',Cost,' stato:', S, '\n']),
	reverse([S|Path], SL),
	states_to_actions(SL, Act),
	write('  azioni:'), writeln(Act).


