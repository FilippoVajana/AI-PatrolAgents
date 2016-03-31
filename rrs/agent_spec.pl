:-module('agent_spec',[]).
:-use_module('../lib/is_a').
:-use_module('../maps/map_spec').

%%%% TYPES
type [clock(number),soldier(position),enemy(position,direction)]: fluent.
%% Fluenti per l'implementazione dell'algoritmo di decisione.

type [movement(direction,position,position),wait]: action.
%% azione che determina la transizione tra fluenti.

%%%% PREDS
pred clock(number).
%% clock(?Time) SEMIDET
%% Spec: vero sse Time è il valore attuale del clock

:-dynamic(clock(T)).

% la add_del_list sarà chiamata solo in seguito ad un'azione dell'agente (o, 
% usando più agenti, in seguito ad una singola azione di ogni agente). Le azioni
% degli agenti causeranno l'aggiornamento del clock e il conseguente spostamento
% dei nemici lungo la loro ronda.
% L'agente stratega assumerà i percorsi dei nemici basandosi sulla direzione in
% cui guardano, ipotizzando 
% Alcune sentinelle avranno comportamenti non determinisitici ma non del tutto
% "liberi" per evitare che una sentinella resti "incastrata" in zone dove non
% dovrebbe trovarsi.
% Esempio di ronda con comportamento non deterministico: in questo caso il
% predicato enemy_path(integer,position) viene esteso con un terzo
% argomento "state" che indica il percorso attualmente seguito (qui s0,s1,s2)
% Il predicato current_state1(?S) indica lo stato corrente dell'automa non
% deterministico che rappresenta la sentinella 1.

enemy_path1(0,p(5,5),s0).	% inizio allo stato zero
enemy_path1(1,p(5,6),s0).	% Procede in linea retta vero (5,9)
enemy_path1(2,p(5,7),s0).
enemy_path1(3,p(5,8),s0).
enemy_path1(4,p(5,9),s1).	% prima deviazione possibile
enemy_path1(5,p(5,10),s1).	% Procede nella stessa direzione fino a (5,12)
enemy_path1(6,p(5,11),s1).	% poi torna indietro
enemy_path1(7,p(5,12),s1).
enemy_path1(8,p(5,11),s1).
enemy_path1(9,p(5,10),s1).	
enemy_path1(4,p(5,9),s2).	% seconda deviazione possibile
enemy_path1(5,p(6,9),s2).	% Gira a destra e prosegue dritto fino a (8,9)
enemy_path1(6,p(7,9),s2).	% poi torna indietro
enemy_path1(7,p(8,9),s2).
enemy_path1(8,p(7,9),s2).
enemy_path1(9,p(6,9),s2).
enemy_path1(10,p(5,9),s0).	% ritorno allo stato zero
enemy_path1(11,p(5,8),s0).	% Ritorna al punto di partenza
enemy_path1(12,p(5,7),s0).
enemy_path1(13,p(5,6),s0).

enemy_path1(K,p(X,Y),State) :-
	K > 13,
	H is K mod 14,
	current_state1(State),
	enemy_path(H,p(X,Y),State).
	
current_state1(s0) :-
	retract(current_state1(_)),
	clock(Time),
	H is Time mod 14,
	not(between(4,9,H)).
current_state1(s1) :-
	retract(current_state1(_)),
	clock(Time),
	H is Time mod 14,
	H == 4,
	random_between(1,2,RandomNumber),
	RandomNumber == 1.
current_state1(s2) :-
	retract(current_state1(_)),
	clock(Time),
	H is Time mod 14,
	H == 4,
	random_between(1,2,RandomNumber),
	RandomNumber == 2.




