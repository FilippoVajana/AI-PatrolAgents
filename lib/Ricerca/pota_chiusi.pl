
:- dynamic(chiuso/1).
%  Usa una base dati dinamica chiuso/1 per memorizzare i nodi
%  chiusi (cioe' gia' incontrati);  non  fa controlli sui costi,
%  ovvero assume la consistenza dell'euristica.
%  NB:   se il grafo � finito e i suoi nodi sono noti in partenza,
%  la chiusura di un nodo si riduce ad un flag associato al nodo;
%  quando il nodo � espanso, si alza il flag; un nodo � chiuso
%  se ha il flag alzato

starting :-
	  retractall(chiuso(_)).


%%  taglia  implementato come taglio dei chiusi
taglia(N,_) :-
    (	showflag -> writeln(verifica_chiusura(N)); true),
    chiuso(N),
    (	showflag -> writeln(N:' � chiuso'); true).

chiudi(N) :-
    (	showflag -> writeln(chiudo(N)); true),
    (	not(chiuso(N)) -> assert(chiuso(N)); true ).

%%	NUOVA VERSIONE DA MIGLIORARE

%chiusura(nc(V,_,_),L,[V|L]).
chiusura(nc(V,_,_),A,B) :-
	put_assoc(V,A,n,B).

potatura(nc(V,_Path,_),Chiusi,dl(L,L)) :-
	get_assoc(V,Chiusi,_),!.
potatura(N,_,dl([N|L],L)).

/*chiusura(nc(V,_,_),_A,_B) :-
	not(chiuso(V)) -> assert(chiuso(V)); true.

potatura(nc(V,_Path,_),_Chiusi,dl(L,L)) :-
	chiuso(V),!.
potatura(N,_,dl([N|L],L)).*/

