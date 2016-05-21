:- discontiguous(ignored(_)).
:- use_module(library(is_a)).
:- use_module(sentinella).


id_campo(1).
area_griglia(area(p(0,5),p(6,0))).
entita_gioco(ostacolo, p(0,0)).
entita_gioco(ostacolo, p(1,0)).
entita_gioco(ostacolo, p(2,0)).
entita_gioco(ostacolo, p(3,0)).
entita_gioco(ostacolo, p(4,0)).
entita_gioco(ostacolo, p(5,0)).
entita_gioco(ostacolo, p(0,1)).
entita_gioco(ostacolo, p(5,1)).
entita_gioco(ostacolo, p(0,2)).
entita_gioco(ostacolo, p(5,2)).
entita_gioco(ostacolo, p(0,3)).
entita_gioco(ostacolo, p(5,3)).
entita_gioco(ostacolo, p(0,4)).
entita_gioco(ostacolo, p(1,4)).
entita_gioco(ostacolo, p(2,4)).
entita_gioco(ostacolo, p(3,4)).
entita_gioco(ostacolo, p(4,4)).
entita_gioco(ostacolo, p(5,4)).

ronda(r1, [p(1,1),p(1,2),p(4,2),p(4,1)]).
sentinella(s1, ronda(r1,PP)) :- ronda(r1,PP).
posizione_sentinella(s1,p(1,1)).
giocatore(g1,p(4,3)).