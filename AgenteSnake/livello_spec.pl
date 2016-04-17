:- module(livello_spec, []).
:- use_module(library(is_a)).
type pred(_,_).
type [[],[T|list(T)]]: list(T).
type number.
type [size(number,number)]:size.
     %  dimensione in (Righe,Colonne) di una mappa rettangolare
     %	con righe da 0 a Righe-1 e colonne da 0 a Colonne-1
type [p(number,number)]:punto.
     %  ogni punto indica un'area quadrata di "terreno"
type [ostacolo]:terreno.
     %   diversi tipi di terreno: ostacoli, p-alude, ' ' libero
type [d(number,number)]: direzione.
     %  d(V,O) con V / O spostamento verticale /orizz. unitario
     %	es.:   d(-1,0)  mi sposto in verticale di -1 punto
type [n,s,e,o,ne,no,se,so]:punto_cardinale.
     % i ben noti punti cardinali
type [area(punto,punto)]:area.
     %  area(P1,P2): area rettangolare con vertice nord-est P1
     %	e vertice sud-ovest P2



pred direzioni(punto_cardinale, direzione).
  %   direzioni(?C, ?D) nondet
  %   D è la direzione corrispondente al punto cardinale C
pred next_dirs(direzione, direzione, direzione).
  %   next_dirs(?D1,?D2,?D3) nondet
  %   D2 e D3 sono le direzioni "prossime" di D1
  %   Ad es. le direzioni prossime di d(1,0) (sud)
  %   sono d(1,-1) (sud-est) e d(1,1) (sud-ovest)
pred next(punto, direzione, punto, number).
  %   next(+P1, +D, ?P2, ?L)  semidet
  %   P2 è il prossimo punto in direzione D da P1
  %   L è la distanza fra P1 e P2
pred next(punto, punto).
  %   next(+P1, ?P2)  semidet
  %   P2 è adiacente a P1
pred adiacenti(punto, list(punto)).
  %   adiacenti(+P, -L)	det
  %   L è la lista dei punti adiacenti a P
pred verso(punto, punto, direzione).
  %  verso(+P1,+P2,?D) semidet
  %  P2 si trova in direzione D rispetto a P1
  %  ad es.  verso(p(3,2), p(1,1), d(-1,-1)):
  %  p(1,1) si trova a nord-est di p(3,2)
pred distanza_euclidea(punto, punto, number).
  %  distanza_euclidea(+P1, +P1, -D) det
  %  D = distanza_euclidea(P1,P2),
  %  usando il lato quadretto come unità di misura
pred distanza_quadretti(punto, punto, number).
  %  distanza_quadretti(+P1, +P1, -D) det
  %  D = lunghezza minimo percorso di un agente che si muove
  %  solo in orizzontale, verticale o diagonale,
  %  usando il lato quadretto come unità di misura
pred punto_area(punto, area).
  %  punto_area(?P,+A) nondet
  %    P si trova nell'area A

pred area_sentinella(punto, sentinella).
  % area_sentinella(?P,+S) nondet
  % P si trova nell'area sorvegliata dalla sentinella S

pred punto_mappa(punto, size).
  %  punto_mappa(?P,+S) nondet
  %  P è un punto in una mappa di dimensione S
pred qualita(terreno, number).
  %  qualita(+T, -K) det.
  %  K = coefficiente difficoltà attraversamento del terreno T

pred carica_mappa.
  %  comando per caricare la mappa corrente
pred mostra_mappa(any, size).
  %  comando:  mostra_mappa(+M, +S)
  %  precondizione:   M è un predicato che rappresenta una
  %  mappa di dimensione Size
  %  post:   la mappa rappresentata da M viene visualizzata
pred mostra_mappa(any).
  %  come sopra ma come size usa quella della mappa caricata

%   DINAMICI, memorizzano la mappa, la posizione attuale
%   dell'agente, la posizione del goal
pred map(punto,terreno).
  %  map(?P,?T) semidet,dynamic: P è un punto della mappa caricata
  %  con tipo di terreno T
pred map_size(size).
  %  map_size(size(?R,?C)) semidet, dynamic:  la mappa caricata
  %  ha R righe e C colonne
pred position(punto).
  %  position(?P): l'agente si trova nella mappa in posizione P
pred goal(punto).
  %  gola(?P): il goal nella mappa è in posizione P

%%  PREDICATI GREZZI da escludere dal type checking:

ignored(ambiente(_)).


