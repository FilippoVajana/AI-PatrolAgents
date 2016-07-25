help_progetto :-
	writeln('*** Per partire con la mappa I usare vai(I)\n*** Le mappe disponibili sono 1,2,3.'),
	writeln('*** help_progetto per help\n\n').
:- writeln(
'\n**************************************************\n
       PROGETTO ESPLORATORE\n
 **************************************************\n'),
help_progetto.

vai(I) :-
	start(mappa(I)).

