:-module('map_spec',[]).
:-use_module('../lib/is_a').

%%%% TYPES
type [p(integer,integer)]: position.
%% Indica la posizione (coordinate X,Y) degli elementi dello scenario.

type [up,down,left,right]: direction.
%% Indica la direzione del movimento o dello sguardo dei personaggi.


%%%% PREDS
pred movement(direction,position,position).                     
%% movement(?Direction,?PositionStart,?PositionEnd) SEMIDET
%% spec: vero sse PositionEnd è la nuova posizione dopo essersi mossi da
%% 	 PositionStart verso la direzione Direction

pred is_game_area(position).
%% is_game_area(?Position) SEMIDET
%% Spec: vero sse Position indica un'area di gioco percorribile

pred is_obstacle(position).
%% is_obstacle(?Position) SEMIDET
%% Spec: vero sse Position indica un ostacolo non percorribile

pred enemy(number,position).
%% enemy(?Time,?Position) SEMIDET
%% Spec: vero sse Position indica un nemico nell'istante Time

:-dynamic(enemy(Int,P)).

pred soldier(position).
%% soldier(?Position) SEMIDET
%% Spec: vero see Position indica la posizione del giocatore

:-dynamic(soldier(P)).

pred prisoner(position).
%% prisoner(?Position) SEMIDET
%% Spec: vero sse Position indica la posizione del prigioniero da raggiungere

pred sight(position).	
%% sight(?Position) SEMIDET
%% Spec: vero sse Position indica una zona tenuta sotto sorveglianza --> possibile aggiunta di tipo sentinel per distinguere le zone coperte dalle varie sentinelle

:-dynamic(sight(P)).

pred enemy_path(integer,position,direction).
%% enemy_path(?Step,?Position,?Direction) SEMIDET
%% Spec: vero sse Position è la posizione assunta da un nemico all'istante Step,
%% mentre Direction è la sua direzione
%% NOTA: si potrebbe creare un tipo "sentinel" per generare le sentinelle e 
%% aggiungere al predicato enemy_path un termine di tipo sentinel che indica
%% a quale sentinella appartiene il percorso --> sembra ragionevole

:-dynamic(enemy_path(Int,P,D)).

pred height(integer).
%% height(?Height) SEMIDET
%% Spec: vero sse Height è l'altezza della mappa

pred size(integer).
%% size(?Size) SEMIDET
%% Spec: vero sse Size è la larghezza della mappa

--> possibile definire un tipo game_map per raggruppare i due precedenti