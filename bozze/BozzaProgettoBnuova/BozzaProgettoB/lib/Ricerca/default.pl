%----------------------------------------
%  DI DEFAULT NON AVVIENE ALCUNA POTATURA
%-----------------------------------------------

%alla partenza non e'necessario far nulla
starting.

best_eq(_,_,_) :- fail.   %% di default non esclude nodi incontrati

taglia(_,_) :- fail.   %% TAGLIO CICLI DISATTIVATO

chiudi(_).   % di default non chiude nodi

potatura(N,_,dl([N|L],L)).
chiusura(_,_,_).
