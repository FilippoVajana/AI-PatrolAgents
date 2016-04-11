
% type format:   vedi i format prolog (dal manuale)
% type term:  un termine qualsiasi
% type win_format(C):   format in cui la tilde è rimpiazzata da C

%  pred win_format(win_format(^), list(term)).
%  win_format(+F,+L) equivale a format(TF,L) dove
%  TF si ottiene da F rimpizzando ^ con tilde
win_format(F,L) :-
	win_format(F,L,^).

%  pred win_format(win_format(char), list(term), char).
%  win_format(+F,+L,+C) equivale a format(CF,L) dove
%  CF si ottiene da F rimpizzando C con tilde
win_format(F,L,C) :-
       atom_codes(C,[CC]),
       atom_codes(F, SF),
       maplist(convert_tilde(CC), SF, ST),
       atom_codes(FT,ST),
       format(FT,L).
convert_tilde(C,C,126):-
	!.
convert_tilde(_,C,C).


%%	indent(+K:number).
%       Scrive nello stream corrente K spazi

indent(K) :-
	forall(between(0,K,_), write(' ')).

%%	write_indented(+K:number, +S:term).
%       Scrive nello stream corrente K spazi seguiti da S
%       senza andare a capo

write_indented(K,S) :-
	indent(K),
	write(S).



