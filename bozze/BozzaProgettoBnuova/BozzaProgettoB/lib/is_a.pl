:- module(is_a, [op(650, fx, type)
		, op(650, fx, pred)
		, op(650, fx, gen)
		, op(650, fx, ignored)
		, op(700, xfx, is_a)
		, typed_module/1
		, is_a/2
		, is_a/3
		, get_error/1
		, type_check/0
		, get_declaration/1
		, get_clause/1
		, help_me/0
		]).

help_me :- maplist(writeln,[
'******************************  HELP:\n',
'  ?X is_a ?T.   per vedere gli X in U(T) per tipi T non ricorsivi',
'  is_a(+K,?X,?T).  per vedere gli X ground in U(T) di complessita'' =< K',
'  get_error(?E).   per ri-vedere gli errori',
'  type_check.      per ri-lanciare il type_checker',
'  get_declartion(?D). per vedere le dichiarazioni riconosciute,',
'  get_clause(?CL). per vedere le clausole con contesto ricostruito',
'  help_me.   per questo help\n',
'***************************************'
	   ]).


:- multifile(false/1).
false(_) :- fail.


/*************************************************************/


/************************  START TYPE CHECKER   ************
 *   Carica in DB segnatura e verifica  le dichiarazioni e esegue il
 *   type checking delle clausole che definscono i predicati dichiarati
 *   **************************************************/

type_check :-
	writeln('\n************************  TYPE CHECKER   ************************\n'),
	catch(do_check,_,
	     writeln('**** TYPE CHECKING ABORTITO,CONTROLLARE LA SINTASSI COMPLESSIVA  ****\n')),
	writeln('\n**********************  FINE TYPE CHECKING **********************\n').

do_check :-
        error(_) ->
	    show_errors,
	    writeln('TYPE CHECKING NON EFFETTUATO'),
	    fail,
	! ;
	clear,
	assert(declared(type(any))),
	%assert(declared(type([[],[T|list(T)]]:list(T)))),
	forall(try(user:type(T)), save_type_decl(T)),
	forall(try(user:gen(G:T)), save_gen_decl(G,T)),
	forall(try(user:pred(P)), save_pred_decl(P)),
	forall(to_check(Head,Body), check_and_save_clause(Head,Body)),
	printerrors,
	forall(false(E), print_error(E)).

%%	  le clausole da sottoporre a type checking sono
%         quelle che definiscono un predicato dichiarato
to_check(Head, Body) :-
	structure(Head,_PSymbol,_Args,_Types),
	user:clause(Head,Body).


/******************************************************
 1.  PREDICATI CARICAMENTO E QUERY DB SEGNATURA
 ******************************************************/

:- dynamic declared/1, reconstructed/1, found/1, warn/1, imported_from/1,
	typed_m/1.

typed_module(M) :-
	assert(typed_m(M)).

%%	clear db
clear :-
	retractall(imported_from(_)),
	retractall(declared(_)),
	retractall(reconstructed(_)),
	retractall(found(_)),
	retractall(warn(_)).

get_declaration(D) :-
	declared(D).

get_clause(C) :-
	reconstructed(C).

%%	saving declarations
save_type_decl(G:T) :- !,
	check_and_do(is_a_new_type(T), assert_last(declared(type(T)))),
	save_gen_decl(G,T).
save_type_decl(T) :-
	check_and_do(is_a_new_type(T), assert_last(declared(type(T)))).
save_gen_decl(G,T) :-
	check_and_do(is_a_gen_decl(G,T), assert(declared(gen(G,T)))).
save_pred_decl(P) :-
	check_and_do(is_a_pred_decl(P), assert(declared(pred(P)))).
check_and_save_clause(Head, Body) :-
	catch(check_clause(Env, Head, Body),
        Error,
       (   assert(found(error(clause(Head,Body), Error))),
	   fail
       ))
       *->  assert(reconstructed(for(Env, clause(Head, Body))))
       ;
       true.

%%	checking before saving
%
is_a_new_type(T) :-
	check_not(declared(type(T)), multiple_declarations_of(T)),
	check(type_constructor(T), error(bad_type_constructor, T)).
is_a_gen_decl(G,T) :-
	check(declared(type(T)), error(generation_type_undefined, G:T)),
	check_not(declared(gen(G1,T)), duplicate_declaration([G1:T, G:T])),
	forall(member(Gen,G), is_a_generator(Gen,T)).
is_a_pred_decl(P) :-
	P =.. [_|Types],
	check_not((member(T,Types), not(declared(type(T)))),
		  error(not_declared_type(T), in:P)).

type_constructor(T) :-
	catch(T =.. [_|Types], _, fail),
	maplist(var, Types).


is_a_generator(Gen,T) :-
	term_variables(T, Vars),
	gen_term(Gen, Vars), !.
is_a_generator({T1},T) :-
	declared(type(T1)),
	term_variables(T1, V1),
	term_variables(T,V2),
	contains_vars(V2,V1), !.
is_a_generator(G,T) :-
	throw(error(bad_generator_declaration, G:T)).

gen_term(Gen, Vars) :-
	catch(Gen =.. [F|_], _, fail),
        function_symbol(F),
	term_variables(Gen, Vars1),
	contains_vars(Vars, Vars1).
contains_vars(_,[]).
contains_vars(Vars, [V|W]) :-
	var_of(V,Vars),!,
	contains_vars(Vars,W).
var_of(V,[X|W]) :-
        X==V, !; var_of(V,W).

function_symbol(X) :-
	atom(X)
	;
	number(X)
	;
	X=[]
	;
	X= '[|]'.




/******************************************************
 2.  QUERY SEGNATURA,  non usa la DB ma le dichiarative
 ******************************************************/

%%	?X is_a ?T vero sse T è ground e X è un termine ground di
%       tipo T NON RICORSIVO; non opera correttamente in presenza
%       di tipi ricorsivi

X is_a T :-
	is_a(100, X, T).

is_a(K, X, T)  :-
	K > 0,
	H is K-1,
	a_type_gen(Gen, T, F, ArgTypes),
	(   (   Gen={T1},
	        is_a(H, X,T1)   )
	;
	    Gen \= {_},maplist(is_a(H), Args, ArgTypes),
	    X =.. [F|Args]
	).

/*a_type_gen([],list(_T),[],[]).
a_type_gen([T|list(T)],list(T),F,ArgTypes) :-
	[T|list(T)] =.. [F|ArgTypes].*/
a_type_gen(Gen, T, F, ArgTypes) :-
	(   try(user:gen(G:T)) ; try(user:type(G:T)) ),
	member(Gen, G),
	not(var(Gen)),
	Gen =.. [F|ArgTypes].

%%	sub_type(?T1,?T2) vero sse T1 è sottotipo di T2
% ----------------------------- DA RIVEDERE CON I costruttori DI TIPO
%

sub_type(T1,T) :-
	T1=T
	;
	(   try(user:gen(G:T)) ; try(user:type(G:T)) ),
	member({TT},G),
	sub_type(T1,TT).



%  potrebbe venir utile rifarla bene
sup_type(T1,T) :-
	a_type_gen(G:TT),
	G={T},
	sup_type(T1,TT).
sup_type(T1,T) :-
	T =.. [TC|Types],
	length(Types, N),
	length(SupTypes, N),
	maplist(sup_type, SupTypes, Types),
	T1 =.. [TC|SupTypes].
sup_type(T1,_T) :-
	var(T1).

a_type(T) :-
	(   try(user:gen(_:T)) ; try(user:type(_:T)) ).
a_type_gen(G:T) :-
	(   try(user:gen(GG:T)) ; try(user:type(GG:T)) ),
	member(G,GG),
	not(var(G)).



comparable(T1,T2) :-
	sub_type(T1,T2) ; sub_type(T2,T1).

structure(A, PredSymbol, Args, Types) :-
	user:pred(P),
	P =.. [PredSymbol|Types],
	length(Types,N),
	length(Args,N),
	A =.. [PredSymbol|Args].

depends(T1, T2) :-
	a_type_gen(_Gen, T1, _F, ArgTypes),
	member(T2, ArgTypes).


/***************************************************
  3. PRIMO FILTRO ERRORI DICHIARATIVE, non usa la DB
     ma le dichiarative
 ***************************************************/

error(duplicate_gen(gen(G1:T), type(G2:T))) :-
	try(user:gen(G1:T)),
	try(user:type(G2:T)).
error(ambiguous_declarations(G:T1, G:T2)) :-
      not(comparable(T1,T2)),
      G is_a T1,
      G is_a T2.
error(undefined_type(T, in:gen(G,T))) :-
	try(user:gen(G:T)),
	not(declared_type(T)).
error(undefined_type(T, in:pred(P))) :-
	try(user:pred(P)),
	P =.. [_|Types],
	member(T, Types),
	not(is_arg_type(T)).
error(a_variable_generator(Gen, Gen:T)) :-
	declared_generator(Gen:T),
	var(Gen).
declared_type(T) :-
	try(user:type(DT)),
	(   DT = _:T ; DT = T ).
is_arg_type(T) :-
	declared_type(T);
	T=number;
	T=any.

declared_generator(G:T) :-
	(   try(user:gen(GG:T)); try(user:type(GG:T))),
	member(G,GG).

/****************************************************
  4. TYPE CHECKING DELLE CLAUSOLE, usa anche la DB,
     oltre alle dichiarative
  ***************************************************/


check_clause(Env, false(_), Body) :- !,
        check_body([],Env,Body).
check_clause(Env, Head, Body) :-
	check_wff([], E1, Head),
        check_body(E1,Env,Body).

check_wff(E1,E2, WFF) :-
	structure(WFF,_PSymbol,Args,Types),
	check_terms(Args, Types, E1,E2).

check_body(E,E, true) :- !.
check_body(E1,E2, (G1,G2)) :- !,
	check_body(E1,E, G1),
	check_body(E,E2, G2).
check_body(E1,E2,(G1;G2)) :- !,
	check_body(E1,E,G1),
	check_body(E,E2,G2).
check_body(E1,E2,(G1 -> G2)) :- !,
	check_body(E1,E,G1),
	check_body(E,E2,G2).
check_body(E1,E2,once(G)) :- !,
	check_body(E1,E2,G).
check_body(E1,E2,not(G)) :- !,
	check_body(E1,E2,G).
check_body(E1,E2,Goal) :-
	check_goal(Goal, E1, E2).

/**************  I GOALS DATALOG  ***********************/

check_goal(G,Env, Env) :-
	try(user:ignored(G)), !.
check_goal(is_a(X,Type),Env1,Env2) :- !,
	declared(type(Type)),
	check_term(X,_,Env1,Env2).
check_goal((X = Y),Env1,Env3) :- !,
	check_term(X,Type1,Env1,Env2),
	check_term(Y,Type2,Env2,Env3),
	Type1=Type2.
check_goal((X \= Y),Env1,Env3) :- !,
	check_term(X,Type,Env1,Env2),
	check_term(Y,Type,Env2,Env3).
check_goal((X == Y),Env1,Env3) :- !,
	check_term(X,Type,Env1,Env2),
	check_term(Y,Type,Env2,Env3).
check_goal((X \== Y),Env1,Env3) :- !,
	check_term(X,Type,Env1,Env2),
	check_term(Y,Type,Env2,Env3).
check_goal((X @< Y),Env1,Env3) :- !,
	check_term(X,Type,Env1,Env2),
	check_term(Y,Type,Env2,Env3).
check_goal((X @> Y),Env1,Env3) :- !,
	check_term(X,Type,Env1,Env2),
	check_term(Y,Type,Env2,Env3).
check_goal((X @>= Y),Env1,Env3) :- !,
	check_term(X,Type,Env1,Env2),
	check_term(Y,Type,Env2,Env3).
check_goal((X @=< Y),Env1,Env3) :- !,
	check_term(X,Type,Env1,Env2),
	check_term(Y,Type,Env2,Env3).
check_goal(copy_term(X,Y),Env1,Env3) :- !,
	check_term(X,Type,Env1,Env2),
	check_term(Y,Type,Env2,Env3).
check_goal((X > Y),Env1,Env3) :- !,
	check_term( X,number,Env1,Env2),
	check_term( Y,number,Env2,Env3).
check_goal((X < Y),Env1,Env3) :- !,
	check_term( X,number,Env1,Env2),
	check_term( Y,number,Env2,Env3).
check_goal((X >= Y),Env1,Env3) :- !,
	check_term( X,number,Env1,Env2),
	check_term( Y,number,Env2,Env3).
check_goal((X =< Y),Env1,Env3) :- !,
	check_term( X,number,Env1,Env2),
	check_term( Y,number,Env2,Env3).
check_goal((X =:= Y),Env1,Env3) :- !,
	check_term( X,number,Env1,Env2),
	check_term( Y,number,Env2,Env3).
check_goal((X =\= Y),Env1,Env3) :- !,
	check_term( X,number,Env1,Env2),
	check_term( Y,number,Env2,Env3).
check_goal(number(I),Env1,Env2) :- !,
	check_term(I,number,Env1,Env2).

check_goal(true,Env1,Env2) :- !, Env1 = Env2.
check_goal(fail,Env1,Env2) :- !, Env1 = Env2.
check_goal(!,Env1,Env2) :- !, Env1 = Env2.
check_goal(abort,Env1,Env2) :- !, Env1 = Env2.
check_goal(V is E,Env1,Env2) :- !,
	check_term(V, number, Env1, Env),
	check_term(E, number, Env, Env2).

check_goal(member(X,L), E1, E2) :-
	declared(type(list(_))),
	check_term(X,Type,E1,E),
	check_term(L, list(Type),E,E2).
check_goal(reverse(L1,L2), E1, E2) :-
	declared(type(list(_))),
	check_term(L1, list(Type),E1,E),
	check_term(L2, list(Type),E,E2).
check_goal(union(L1,L2,L3), E1, E2) :-
	declared(type(list(_))),
	check_term(L1, list(Type),E1,EA),
	check_term(L2, list(Type),EA,EB),
	check_term(L3, list(Type),EB,E2).
check_goal(intersection(L1,L2,L3), E1, E2) :-
	declared(type(list(_))),
	check_term(L1, list(Type),E1,EA),
	check_term(L2, list(Type),EA,EB),
	check_term(L3, list(Type),EB,E2).
check_goal(append(L1,L2,L3), E1, E2) :-
	declared(type(list(_))),
	check_term(L1, list(Type),E1,EA),
	check_term(L2, list(Type),EA,EB),
	check_term(L3, list(Type),EB,E2).

check_goal(between(I,J,K),Env1,Env2) :- !,
	check_term(I, number, Env1, Enva),
	check_term(J, number, Enva, Envb),
	check_term(K, number, Envb, Env2).
check_goal(sign(I,J),Env1,Env2) :- !,
	check_term(I, number, Env1, Env),
	check_term(J, number, Env, Env2).
check_goal(throw(_Err),Env,Env) :- !.
check_goal(catch(Goal,_,Handler),Env1,Env3) :- !,
	check_goal(Goal,Env1,Env2),
	check_goal(Handler,Env2,Env3).

check_goal(assert(Goal),Env1,Env2) :- !,
	check_goal(Goal,Env1,Env2).
check_goal(retract(Goal),Env1,Env2) :- !,
	check_goal(Goal,Env1,Env2).
check_goal(retractall(Goal),Env1,Env2) :- !,
	check_goal(Goal,Env1,Env2).
check_goal(::(_,_), Env, Env) :- !.
check_goal(write(_), Env, Env) :- !.
check_goal(writeln(_), Env, Env) :- !.
check_goal(write(_,_), Env, Env) :- !.
check_goal(writeln(_,_), Env, Env) :- !.
check_goal(nl, Env, Env) :- !.
check_goal(setof(Term,Body,List), Env1,Env2) :- !,
	check_term(Term,Type,Env1,EnvA),
	check_body(EnvA, EnvB, Body),
	check_term(List,list(Type),EnvB,Env2).




check_goal(Goal,Env1,Env2) :-
	structure(Goal,_Rel, Args, Arity), !,
	check_terms( Args, Arity,Env1, Env2).

check_goal(Goal,Env,Env) :-
	/* all other predicates are simply ignored */
	assert(warn(unknown_predicate_call(Goal))).

check_terms([],[], E,E).
check_terms([A|Args], [T|Types], E1,E2) :-
	check_term(A,T, E1,E),
	check_terms(Args, Types, E,E2).

check_term(_X, Type, E, E) :- Type==any, !.
check_term(X, Type, E1,E2) :-
	var(X),!,
	insert_var(X:Type,E1,E2).
check_term(L, list(_), E, E) :-
	L==[], !,
	declared(type(list(_))).
check_term(L, list(_), E1, E2) :-
	not(var(L)),
	L = [T|LL], !,
	declared(type(list(_))),
	check_term(T,Type, E1, E),
	list_term(LL, list(Type),E,E2).
check_term(T, number, E1,E2) :-
	numeric_term(T,_Op, Args,Types),!,
	check_terms(Args, Types, E1, E2).
check_term(C, Type, E1,E2) :-
	(   C=.. [F|Args],
	    check_terms(Args, Types, E1, E2),
	    length(Args,N),
	    length(Vars,N),
	    Gen =..[F|Vars],
	    a_type_gen(Gen, Type, F, Types)
	 ;
	    a_type_gen({T1}, Type, _, _),
	    catch(check_term(C, T1, E1, E2),_,fail)
	 %;
	 %   Type=any
	)
	*-> true
	;
	throw(error(term_error, C:Type)).

insert_var(V:T1,[W:T2|E],[V:T3|E]) :-
	V==W, !,
	(   min_type(T1,T2,T3) ->
	     true
	     ;
	     throw(error(type_clash, [V:T1, V:T2]))).
insert_var(V:T1, [D|E], [D|E1]):-
	insert_var(V:T1, E, E1).
insert_var(V:Type,[],[V:Type]).

%%	TODO
min_type(T,T,T) :-!.
min_type(T1,T2,T1) :-
	sub_type(T1,T2),!.
min_type(T1,T2,T2) :-
	sub_type(T2,T1).



list_term([],list(_), Env,Env).
list_term(L, list(Type), Env1,Env2) :-
	not(var(L)),
	L = [T|LL],
	check_term(T, Type, Env1, Env),
	list_tail(LL, list(Type), Env, Env2).
list_tail(V, list(T), Env1, Env2) :-
	var(V),!,
	insert_var(V:list(T),Env1,Env2).
list_tail([T|L], list(T), Env1, Env2) :-
	check_term(T, Type, Env1, Env),
	list_tail(L, list(Type),Env,Env2).

numeric_term(C, C, [], []) :- number(C), !.
numeric_term(X+Y, +, [X,Y], [number,number]).
numeric_term(X-Y, -, [X,Y], [number,number]).
numeric_term(X*Y, *, [X,Y], [number,number]).
numeric_term(X/Y, /, [X,Y], [number,number]).
numeric_term(X // Y, //, [X,Y], [number,number]).
numeric_term(X mod Y, mod, [X,Y], [number,number]).
numeric_term(X div Y, div, [X,Y], [number,number]).
numeric_term(X ^ Y, ^, [X,Y], [number,number]).
numeric_term(min(X,Y), min, [X,Y], [number,number]).
numeric_term(max(X,Y), max, [X,Y], [number,number]).
numeric_term(abs(X), abs, [X], [number]).
numeric_term(sqrt(X), sqrt, [X], [number]).
numeric_term(random(X), random, [X], [number]).


/*********************************************************
 * 5. VISUALIZZAZIONE ERRORI
 * *******************************************************/

printerrors :-

	forall(found(Error), print_error(Error)),
	forall(warn(Warning), print_warning(Warning)),
	(   found(_) ->
	    maplist( writeln,
		     [
	       '\nTROVATI DEGLI ERRORI'
	     , 'puoi rivederli con get_error(E).'
	     , 'Per rivedere le dichiarazioni esaminate: get_declaration(D).'
	     , 'Per rivedere le clausole ricostruite: get_clause(CL).'
	     , 'Per un HELP: help_me.\n'
		     ])%,
	    %fail
	    ;
	    maplist(writeln,
		    [
	      'NESSUN ERRORE INDIVIDUATO'
	    , 'Per rivedere le dichiarazioni esaminate: get_declaration(D).'
	    , 'Per rivedere le clausole ricostruite: get_clause(CL).'
	    , 'Per un HELP: help_me.\n'
		    ])
	).

show_errors :-
	forall(error(E), print_error(E)).

get_error(E) :-
	error(E),
	print_error(E)
	;
	found(E),
	print_error(E)
	;
	warn(E),
	print_warning(E).

print_error(E) :- writeln('*************':E).
print_warning(W) :- writeln('WARNING ':W).


/*********************** UTILITY **********************/

try(P) :-
	%  instead of throwing an exception
	%  fails
	catch(P,_,fail).

check(P,Err) :-
	% throws Err if P fails
	try(call(P)),!; treat_error(Err).
check_not(P,Err) :-
	% throws Err if P succeeds
	call(P), !, treat_error(Err); true.

treat_error(error(E,M)) :- !,
	throw(error(E,M)).
treat_error(W) :-
	assert(warn(W)).

check_and_do(Check, Do) :-
	catch(call(Check),Error,save_error(Error)),
	Do.
save_error(Error) :-
	assert(found(Error)).

assert_last(A) :-
	retractall(A),
	assert(A).







