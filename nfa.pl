:-use_module('lib/is_a').

% dimensione mappa
mapsize(20,30).

type [s1]: sentinel.
type [p(integer,integer)]: coord.
type [up,down,left,right]: direction.
type [q0,q1,q2]: state.

pred ronda(sentinel,coord,direction,integer,state).
%% ronda(?Sentinel,?Position,?Direction,?Instant) SEMIDET
%% Spec: vero sse Sentinel si trova in Position,Direction all'istante Instant

pred avanza.
%% avanza DET
%% Spec: da chiamare per far avanzare il tempo

pred start.
%% start DET
%% Spec: da chiamare all'inizio

pred sentinella(sentinel,coord,direction).
%% sentinella(?Sent,?Coord,?Direction) SEMIDET
%% Spec: vero sse la sentinella Sent si trova attualmente in <Coord,Direction>

:-dynamic(sentinella(_,_,_)).

pred clock(integer).
%% clock(-Time) DET
%% Spec: Time indica l'istante attuale

:-dynamic(clock(_)).

pred current_state(sentinel,state).
%% current_state(?Sentinella,?Stato) SEMIDET
%% Spec: vero sse Stato è lo stato attuale di Sentinella

:-dynamic(current_state(_,_)).

clock(0).
sentinella(s1,p(10,10),up).
current_state(s1,q0).

start :-
	retract(sentinella(_,_,_)),
	assert(sentinella(s1,p(10,10),up)),
	retract(clock(_)),
	assert(clock(0)).
	
print_status :-
	clock(Time),
	sentinella(Sentinel,p(X,Y),Direction),
	format("Ore ~p~n",[Time]),
	format("Sentinella ~p: (~p,~p), ~p~n",[Sentinel,X,Y,Direction]).
	
avanza :-
	clock(Time),
	Next is Time + 1,
	retract(clock(Time)),
	retract(sentinella(s1,_,_)),
	assert(clock(Next)),
	ronda(Next,s1,Posizione,Direzione,_),
	assert(sentinella(s1,Posizione,Direzione)),
	retract(current_state(Before,_)),
	next_state(s1,Before,Now),
	assert(current_state(s1,Now)),
	print_status.
	
ronda(0,s1,p(10,10),up,q0).
ronda(1,s1,p(10,11),up,q0).
ronda(2,s1,p(10,12),up,q0).
ronda(3,s1,p(10,13),up,q0).
ronda(4,s1,p(10,14),up,q1).
ronda(5,s1,p(10,15),up,q1).
ronda(6,s1,p(10,16),up,q1).
ronda(7,s1,p(10,17),up,q1).
ronda(8,s1,p(10,17),right,q1).
ronda(9,s1,p(10,16),down,q1).
ronda(10,s1,p(10,15),down,q1).
ronda(11,s1,p(10,14),down,q1).
ronda(4,s1,p(11,13),right,q2).
ronda(5,s1,p(12,13),right,q2).
ronda(6,s1,p(13,13),right,q2).
ronda(7,s1,p(14,13),right,q2).
ronda(8,s1,p(14,13),down,q2).
ronda(9,s1,p(13,13),left,q2).
ronda(10,s1,p(12,13),left,q2).
ronda(11,s1,p(11,13),left,q2).
ronda(12,s1,p(10,13),down,q0).
ronda(13,s1,p(10,12),down,q0).
ronda(14,s1,p(10,11),down,q0).
ronda(15,s1,p(10,10),down,q0).
ronda(K,s1,Position,Direction,State) :-
	K > 15,
	H is K mod 16,
	current_state(s1,State),
	ronda(H,s1,Position,Direction,State).
	
next_state(s1,q1,q0) :-
	clock(T),
	H is T mod 16,
	H == 12, !.
next_state(s1,q2,q0) :-
	clock(T),
	H is T mod 16,
	H == 12, !.
next_state(s1,q0,Q) :-
	clock(T),
	H is T mod 16,
	H == 4,
	random_between(1,2,RandomNumber),
	(RandomNumber == 1, Q is q1 ; RandomNumber == 2, Q is q2), !.
next_state(s1,Q,Q) :-
	current_state(s1,Q).
	
	
	