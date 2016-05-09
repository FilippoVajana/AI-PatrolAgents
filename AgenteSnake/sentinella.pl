:- discontiguous(ignored(_)).
:- use_module(library(is_a)).

type [p(number,number)]:punto.
     %  ogni punto indica un'area quadrata di "terreno"
type [area(punto,punto)]:area.
     %  area(P1,P2): area rettangolare con vertice nord-est P1
     %	e vertice sud-ovest P2
type [nord,sud,est,ovest,center]:punto_cardinale.
     % i 4 ben noti punti cardinali

pred ronda(id, list(punto)).
	%ronda(ID, ?LP) det
	%definisce un percorso di ronda attraverso i suoi punti di passaggio
:- dynamic ronda/2.

pred sentinella(id, ronda).
	%sentinella(+ID,?R) definisce una sentinella tramite nome e e identificativo della ronda in corso
:- dynamic sentinella/2.

pred posizione_sentinella(sentinella, punto).
	%posizione_sentinella(+S,+P)
:- dynamic posizione_sentinella/2.

%%	Utile per migliorare la definizione dell'area di attenzione (area_sentinella)
pred direzione_cammino_sentinella(id_sentinella, punto_cardinale).
	%direzione_cammino_sentinella(+ID_S,-DS)
	%restituisce la direzione in cui la sentinella S sta pattugliando/guardando

pred giocatore(nome, punto).
	%giocatore(+G,+POS)
	%descrive un giocatore tramite nome e posizione

pred giocatore_avvistato(id_giocatore, id_sentinella).
	%giocatore_avvistato(+G,?S)
	%rileva se il giocatore G è stato avvistato da una sentinella S

pred sentinella_avanza(id_sentinella).
	%sentinella_avanza(+ID_S)
	%fa avanzare la sentinella ID_S lungo il suo percorso di ronda

pred aggiorna_ronda(id_sentinella).
	%aggiorna_ronda(+S)
	%verifica se è il caso di aggiornare la lista dei punti di passaggio della ronda

pred clock(integer).
	%predicato dinamico che identifica l'istante temporale della simulazione
:- dynamic clock/1.

pred aggiorna_clock().
	%fa avanzare di uno step il tempo della simulazione

pred azzera_clock().
	%porta a 0 il tempo della simulazione




		%%	IMPLEMENTAZIONI	%%

%%	CLOCK

clock(0).

aggiorna_clock() :- 
	clock(Time),
	New_Time is (Time + 1),
	retractall(clock(_)),
	assertz(clock(New_Time)).

azzera_clock() :-
	retractall(clock(_)),
	assertz(clock(0)).

%%########################################################################%%


aggiorna_ronda(ID_S) :-
	sentinella(ID_S, ronda(ID_R, [p(XR,YR)|PP_Tail])),
	posizione_sentinella(ID_S,p(XS,YS)),
	( 	(XS - XR + YS - YR) =:= 0 ->
			%aggiorno ronda
			append(PP_Tail, [p(XR,YR)], New_PP),
			retractall(ronda(ID_R, [p(XR,YR)|PP_Tail])),
			assertz(ronda(ID_R,New_PP)),
			%% da togliere %%			
			write("DEBUG sentinella.pl - Aggiornata Ronda\t"),
			write(ronda(ID_R,New_PP)),
			true 			
			;
			true 
	),
	true. %%rivedere

sentinella_avanza(ID_S) :-
	sentinella(ID_S, ronda(ID_R, [PP_Head|PP_Tail])),
	posizione_sentinella(ID_S, p(XS,YS)),
	%aggiorno la lista dei punti di passaggio per la ronda
	aggiorna_ronda(ID_S),

	%%if
	(
		direzione_cammino_sentinella(ID_S, nord) ->
			%aggiorno la posizione della sentinella	
			New_YS is (YS + 1),
			retractall(posizione_sentinella(ID_S,_)),
			assertz(posizione_sentinella(ID_S, p(XS,New_YS)))
		;
		direzione_cammino_sentinella(ID_S, sud) ->
			%aggiorno la posizione della sentinella	
			New_YS is (YS - 1),
			retractall(posizione_sentinella(ID_S,_)),
			assertz(posizione_sentinella(ID_S, p(XS,New_YS)))
		;
		direzione_cammino_sentinella(ID_S, est) ->
			%aggiorno la posizione della sentinella	
			New_XS is (XS + 1),
			retractall(posizione_sentinella(ID_S,_)),
			assertz(posizione_sentinella(ID_S, p(New_XS,YS)))
		;
		direzione_cammino_sentinella(ID_S, ovest) ->
			%aggiorno la posizione della sentinella	
			New_XS is (XS - 1),
			retractall(posizione_sentinella(ID_S,_)),
			assertz(posizione_sentinella(ID_S, p(New_XS,YS)))		
	).

ronda(r1, [p(0,0),p(5,0),p(5,5),p(0,5)]).
ronda(r2, [p(0,0),p(15,0),p(15,15),p(0,15)]).

sentinella(s1, ronda(r1,PP)) :- ronda(r1,PP).% da rivedere per benino
sentinella(s2, ronda(r2,PP)) :- ronda(r2,PP).% eventualmente rimuovere ronda() a favore di id_ronda

posizione_sentinella(s1,p(0,0)).% posizione attuale della sentinella S
posizione_sentinella(s2,p(0,0)).% la posizione iniziale viene impostata dal programma di generazione della griglia

direzione_cammino_sentinella(ID_S, center) :-
	posizione_sentinella(ID_S, p(X_S,Y_S)),
	sentinella(ID_S,ronda(_,[p(X_P,Y_P)|_])),
	Y_P =:= Y_S, 
	X_P =:= X_S.	
direzione_cammino_sentinella(ID_S, nord) :-
	posizione_sentinella(ID_S, p(X_S,Y_S)),
	sentinella(ID_S,ronda(_,[p(X_P,Y_P)|_])),
	Y_P @> Y_S.
direzione_cammino_sentinella(ID_S, sud) :-
	posizione_sentinella(ID_S, p(X_S,Y_S)),
	sentinella(ID_S,ronda(_,[p(X_P,Y_P)|_])),
	Y_P @< Y_S.
direzione_cammino_sentinella(ID_S, ovest) :-
	posizione_sentinella(ID_S, p(X_S,Y_S)),
	sentinella(ID_S,ronda(_,[p(X_P,Y_P)|_])),
	X_P @< X_S.
direzione_cammino_sentinella(ID_S, est) :-
	posizione_sentinella(ID_S, p(X_S,Y_S)),
	sentinella(ID_S,ronda(_,[p(X_P,Y_P)|_])),
	X_P @> X_S.

giocatore(g1, p(5,2)).
giocatore(g2, p(0,0)).
giocatore(g3, p(100,100)).

giocatore_avvistato(ID_G, ID_S) :-
	giocatore(ID_G, p(X_G,Y_G)),
	posizione_sentinella(ID_S, p(X_S,Y_S)),
	area_sentinella(p(X_S,Y_S), A),
	punto_area(p(X_G, Y_G), A).
	




%% PREDICATI VECCHI %%

/*
aggiorna_ronda(ID_S) :-
	sentinella(ID_S, ronda(ID_R, [p(XR,YR)|PP_Tail])),
	posizione_sentinella(ID_S,p(XS,YS)),
	%verifico che la sentinella sia in un punto di passaggio
	(XS + YS - XR - YR) =:= 0,
	%aggiorno ronda	
	append(PP_Tail, [p(XR,YR)], New_PP),
	assertz(ronda(ID_R,New_PP)),
	retractall(ronda(ID_R, [p(XR,YR)|PP_Tail])),
	true.
*/
/*
%avanzamento verso nord
sentinella_avanza(ID_S) :-
	sentinella(ID_S, ronda(ID_R, [PP_Head|PP_Tail])),
	posizione_sentinella(ID_S, p(XS,YS)),

	%aggiorno la lista dei punti di passaggio per la ronda
	aggiorna_ronda(ID_S),

	%la direzione della sentinella è discriminante per le operazioni seguenti		
	direzione_cammino_sentinella(ID_S, nord),
		
	%aggiorno la posizione della sentinella	
	New_YS is (YS + 1),
	retractall(posizione_sentinella(ID_S,_)), %elimino vecchia posizione
	assertz(posizione_sentinella(ID_S, p(XS,New_YS))), %salvo nuova posizione	
	
	%il tempo avanza
	aggiorna_clock().
*/


%%	livello_spec.pl	%%

%%  PREDICATI RELATIVI A SENTINELLA  %%

pred area_sentinella(sentinella, area).
  % area_sentinella(+S,-A) nondet
  % ritorna l'area coperta dalla sentinella S

pred punto_area(punto, area).
  %  punto_area(?P,+A) nondet
  %    P si trova nell'area A


%%	livello.pl	%%

%% PREDICATI RELATIVI A SENTINELLA  %%

punto_area(p(I,J),area(p(I0,J0),p(I1,J1))) :-
	between(I0,I1,I),
	between(J1,J0,J).

area_sentinella(p(I,J), area(p(X1,Y1),p(X2,Y2))) :-
	X1 is I - 2,
	Y1 is J + 2,
	X2 is I + 2,
	Y2 is J - 2. %l'area è un quadrato 4X4 con al centro la sentinella


%%	progetto_snake.pl	%%

%% PREDICATI RELATIVI A SENTINELLA  %%

pred avvistato(giocatore, sentinella).
%%	avvistato(-S) SEMIDET
%%	Spec: vero sse S Ã¨ la sentinella che ha avvistato l'agente

avvistato(p(SX,SY),p(SENTX,SENTY),NAME) :-
  sentinella_dove(p(SENTX,SENTY),_,NAME), %% NOTA da implementare
  area_sentinella(p(SENTX,SENTY),NAME, area(p(X1,Y1),p(X2,Y2))),
  punto_area(p(SX,SY),area(p(X1,Y1),p(X2,Y2))).

  %	Snake viene avvistato se risulta essere nell'area di una sentinella