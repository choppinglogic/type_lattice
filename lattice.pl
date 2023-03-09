%Sample knowledge base. Use asserta(related(c,c)) to test reflexive_relation
:- dynamic related/2.
related(a,b).
related(a,a).
related(c,b).

%Tests if list is a set.
set([]).
set([H|T]) :-
    \+ member(H,T),
    set(T).

%Finds all binary relations with a given functor name in the knowledge base.
find_all_binary_relations(FunctorName,TermList) :-
    Term =.. [FunctorName|[_,_]],
    findall(Term,Term,TermList).

%Tests whether a list of binary relations is reflexive.
reflexive_list([]).
%Case 1: Term in list is already reflexive and can be skipped.
reflexive_list([Term|Terms]) :-
    Term =.. [_|[Arg1,Arg2]],
    Arg1 = Arg2,
    reflexive_list(Terms).
%Case 2: Term in list is not reflexive; reflexive term must be constructed and called.
reflexive_list([Term|Terms]) :-
    Term =.. [FunctorName|[Arg1,Arg2]],
    \+ (Arg1 = Arg2),
    ReflexiveTerm =.. [FunctorName|[Arg1,Arg1]],
    call(ReflexiveTerm),
    reflexive_list(Terms).

%Tests whether a binary relation is reflexive by specifying a function name and invoking macros to find all instances of binary terms with that name.
reflexive_relation(FunctorName) :-
    find_all_binary_relations(FunctorName,TermList),
    reflexive_list(TermList).
