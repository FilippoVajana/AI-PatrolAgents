:- consult(frontiera_ord).

%%%  STRATEGIA costo minimo (lowest cost first), detta anche
%    costo uniforme, ordinamento frontiera_ord in base al costo
%
leq(nc(_,_,Costo1),nc(_,_,Costo2)) :-
       Costo1 =< Costo2.

priority(nc(_,_,Costo),Costo).




