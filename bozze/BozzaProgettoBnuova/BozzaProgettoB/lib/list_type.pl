:- module(list_type, []).

type [[],[T|list(T)]]: list(T).

pred member(T,list(T)).
pred reverse(list(T), list(T)).
pred union(list(T), list(T), list(T)).
pred intersection(list(T), list(T), list(T)).
pred append(list(T), list(T), list(T)).
pred union(list(T), list(T), list(T)).
pred append(list(list(T)), list(T)).


