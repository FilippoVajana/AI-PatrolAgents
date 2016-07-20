:- use_module(sentinella).

:-assert(sentinella(s1,ronda(r1,[p(9,2),p(9,5)]))).
:-assert(sentinella(s2,ronda(r2,[p(2,10),p(5,10),p(5,7),p(2,7)]))).
:-assert(posizione_sentinella(s1,p(9,2))).
:-assert(posizione_sentinella(s2,p(2,10))).
