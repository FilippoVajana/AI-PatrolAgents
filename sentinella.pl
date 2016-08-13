:- module('sentinella',
	  [stato_sentinella/3,
	   soldato_avvistato/3,
	   area_sentinella/3,
	   carica_ronda/1,
	   ronda_sentinella/2,
	   ronda_corrente/1]).
:- discontiguous(ignored(_)).
:- discontiguous sentinella: (pred)/1.
:- use_module(library(is_a)).
:- use_module(livello_spec).
:- use_module(tempo).

type [s1,s2,s3,s4]: id_sentinella.
type [r1,r2,r3,r4]: id_ronda.

pred stato_sentinella(id_sentinella,punto,punto_cardinale).
%%	stato_sentinella(?Id,?Punto,?Dir) SEMIDET
%%	Spec: vero sse la sentinella Id si trova attualmente nel punto
%	Punto e guarda verso Dir

pred soldato_avvistato(id_sentinella,punto,tempo).
	%soldato_avvistato(?S,?P,?T)
	%rileva se il giocatore e' stato avvistato da una sentinella S mentre si trova nel punto P al tempo T



stato_sentinella(IdSentinella,Posizione,Direzione) :-
	clock(T),
	ronda_sentinella(IdSentinella,IdRonda),
	ronda(IdRonda,Posizione,Direzione,T).

soldato_avvistato(S,p(X_G,Y_G),T) :-
	ronda_sentinella(S,R),
	ronda(R, p(X_S,Y_S), Direzione, T),
	area_sentinella(p(X_S,Y_S), Direzione, A),
	punto_area(p(X_G, Y_G), A),
	writeln('[':S:']: avvistato!').

pred ronda_sentinella(id_sentinella,id_ronda).
%%	ronda_sentinella(?Sentinella,?Ronda) SEMIDET
%%	Spec: vero sse Ronda e' la ronda seguita da Sentinella
ronda_sentinella(s1,r1).
ronda_sentinella(s2,r2).
ronda_sentinella(s3,r3).
ronda_sentinella(s4,r4).

pred ronda(id_ronda,punto,direzione,tempo).
%%	ronda(?R,?P,?D,?T) SEMIDET
%%	Spec: vero sse la ronda R, al tempo T, prevede che la sentinella
%	si trovi in posizione P e direzione D.

pred ronda_corrente(string).
%%	ronda_corrente(--FileName) DET
%%	Spec: contiene il filename del set di ronde attualmente caricato
:- dynamic(ronda_corrente/1).

pred carica_ronda(integer).
%%	carica_ronda(++I) DET
%%	Spec: carica il set di ronde specificato da I
carica_ronda(I) :-
	(
        ronda_corrente(VecchiaRonda),
	unload_file(VecchiaRonda)
	;
	assert(ronda_corrente('nessuna'))
	),
	nomi_ronde(I,File),
	consult(File),
	retractall(ronda_corrente(_)),
	assert(ronda_corrente(File)).

pred nomi_ronde(integer,string).
%%	nomi_ronde(?I,?File) DET
%%	Spec: mappa i numeri dei set di ronde sui rispettivi filename.
nomi_ronde(1,'ronde_1').
nomi_ronde(2,'ronde_2').
nomi_ronde(3,'ronde_3').
nomi_ronde(4,'ronde_4').
nomi_ronde(5,'ronde_5').

%%  PREDICATI RELATIVI A SENTINELLA  %%

pred area_sentinella(sentinella, punto_cardinale, area).
  % area_sentinella(+S,+P,-A) nondet
  % ritorna l'area coperta dalla sentinella S che guarda verso il punto cardinale P

pred area_sentinella(id_sentinella, direzione, area).
  % area_sentinella(+S,+D,-A) DET
  % ritorna l'area coperta dalla sentinella S che guarda verso la direzione D

%% PREDICATI RELATIVI A SENTINELLA  %%

area_sentinella(P,Dir,A) :-
	direzioni(PC,Dir),
	area_sentinella(P,PC,A).
area_sentinella(p(I,J),n,area(p(I1,J1),p(I2,J2))) :-
	I1 is I - 3,
	J1 is J + 1,
	I2 is I + 1,
	J2 is J - 1.
area_sentinella(p(I,J),s,area(p(I1,J1),p(I2,J2))) :-
	I1 is I - 1,
	J1 is J + 1,
	I2 is I + 3,
	J2 is J - 1.
area_sentinella(p(I,J),o,area(p(I1,J1),p(I2,J2))) :-
	I1 is I - 1,
	J1 is J + 1,
	I2 is I + 1,
	J2 is J - 3.
area_sentinella(p(I,J),e,area(p(I1,J1),p(I2,J2))) :-
	I1 is I - 1,
	J1 is J + 3,
	I2 is I + 1,
	J2 is J - 1.
