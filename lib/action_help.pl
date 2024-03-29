:- module(action_help, [action_help/0]).

:- use_module(util).


action_help :-
	forall(clause(action_help(Command), Body),
	       (   write(Command),
	           write(': '),
	           Body)).

action_help('action_if.pl') :- writeln(
'Per usare action e'' necessario implementare quanto
          specificato nel modulo di interfaccia action_if.
I predicati messi a disposizione da action sono i seguenti.\n').

action_help(default_strategy) :- writeln(
'da usare prima di tutto per caricare
          la strategia di default A*+pota_chiusi;
	  ma si pu� scegliere anche un''altra strategia').

action_help('solution(-Sol, -Cost)') :- writeln(
'Sol � un piano di costo Cost
	  che risolve l''istanza di problema data dai predicati
	  starting_state(-Statoiniziale)
	  trovato(?Goal)
	  add_del per le azioni').

action_help(exec) :- writeln(
'calcola la prima soluzione e chiede se si vuol
	  vedere la simulazione della esecuzione; rispondendo
	  s  (per si''), viene mostrata l''esecuzione del piano').

action_help('PER LE STATISTICHE') :- writeln(
' usare stats_on per attivare le statistiche del modulo ricerca
         (per i dettagli, search_help).
         usare time(solution(Sol,Cost))  per avere i tempi di
	 esecuzione da Prolog').

action_help('SOLO PER IL DEBUG') :- writeln(
'si pu� vedere come A* cerca il piano attivando l''esecuzione con
         traccia, comando show.  Per i dettagli vedere search_help.').

action_help('         NB.  mostra_nodo') :- writeln(
'la visualizzazione dei nodi della frontiera in fase
         di debug avvieneusando mostra_nodo,che estrae dal nodo
	 nc(S,Path, Cost) il piano corrispondente;
	 personalizzando mostra_nodo personalizzate le stampe di debug').


