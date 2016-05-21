%% PRIMA BOZZA PER LA GESTIONE E LA STAMPA DEL CAMPO DI GIOCO %%
:- discontiguous(ignored(_)).
:- use_module(library(is_a)).
:- module(campo_gioco_spec, []).
%%Importo da sentinella
:- use_module(sentinella).

%%type [p(number,number)]:punto.
     %  ogni punto indica un'area quadrata di "terreno"
%%type [area(punto,punto)]:area.
     %  area(P1,P2): area rettangolare con vertice nord-est P1
     %	e vertice sud-ovest P2

type [giocatore, sentinella, ostacolo, obiettivo]:entita.

%%Informazioni sul campo

pred id_campo(number).
	%id univoco per il campo_gioco
	%impostato dal PGG

pred area_griglia(area).
	%dimensioni del campo da gioco
	%impostato dal PGG

%%Fine Informazioni 


pred entita_gioco(entita, punto).
	%identifica un oggetto nella griglia
	%impostato dal PGG

pred is_ostacolo(punto).
	%true se nel punto P vi è un ostacolo
is_ostacolo(P) :- entita_gioco(ostacolo, P).

pred in_griglia(punto).
	%true se il punto si trova nel campo da gioco
in_griglia(p(X,Y)) :- area_griglia(area(p(Xne,Yne),p(Xso,Yso))),
						between(Xso,Xne,X),
						between(Yso,Yne,Y).
