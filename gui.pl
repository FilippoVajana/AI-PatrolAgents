:-module('gui',[dimensioni_finestra/4,
	        map_refresh/1]).
:-use_module(library(pce)).
:-use_module(library(is_a)).
:-use_module(sentinella).
:-ronda_corrente(RC),consult(RC).

pred dimensioni_finestra(integer,integer,integer,integer).
%%	dimensioni_finestra(+R,+C,-W,-L) DET
%%	Spec: riceve le dimensioni in caselle e le restituisce in pixel

dimensioni_finestra(R,C,X,Y):-
	X is C * 48,
	Y is R * 48.

pred get_sprite(punto, sprite_type).
%%	get_sprite(+P,-S) DET
%%	Spec: dato un punto della mappa, restituisce un atomo
%	corrispondente al tipo di sprite da disegnare nella finestra.
get_sprite(P,'sprites/player.bmp') :-
	position(P), !.
get_sprite(P,'sprites/prisoner.bmp') :-
	goal(P), !.
get_sprite(P, 'sprites/sent-up.bmp') :-
	stato_sentinella(_,P,n), !.
get_sprite(P, 'sprites/sent-down.bmp') :-
	stato_sentinella(_,P,s), !.
get_sprite(P, 'sprites/sent-left.bmp') :-
	stato_sentinella(_,P,o), !.
get_sprite(P, 'sprites/sent-right.bmp') :-
	stato_sentinella(_,P,e), !.
get_sprite(P, 'sprites/wall.bmp') :-
	map(P,o), !.
get_sprite(P, 'sprites/patrolled.bmp') :-
	bagof(Sent, (
		  stato_sentinella(Sent,Ps,D),
		  area_sentinella(Ps,D,A),
		  punto_area(P,A)
	      ), _),
	% fallisce se il risultato di bagof e' una lista vuota, quindi se non e' fallito metto il cut
	!.
get_sprite(P, 'sprites/grass.bmp') :-
	map(P, ' ').

pred map_refresh(picture).
%%	map_refresh(++Picture) DET
%%	Spec: aggiorna la finestra
map_refresh(Finestra) :-
	map_size(Size),
	setof(sprite(P,SpriteType), (punto_mappa(P,Size), get_sprite(P,SpriteType)), ListaSprite),
	forall(member(sprite(p(X1,Y1),Sprite),ListaSprite),
	       (   dimensioni_finestra(X1,Y1,X2,Y2),
		   send(Finestra, display, new(_, bitmap(Sprite)), point(X2,Y2)))).


