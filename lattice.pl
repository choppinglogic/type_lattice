%Sample knowledge base. Use "asserta(related(c,c))" for instance to test "reflexive_relation(related)" - should evaluate to "true."
:- dynamic related/2.
related(a,b).
related(a,a).
related(b,b).
related(b,c).

%Finds all binary relations with a given functor name in the knowledge base.
find_all_binary_relations(FunctorName,TermList) :-
    Term =.. [FunctorName|[_,_]],
    findall(Term,Term,TermList).

%Tests to ensure that when X is related to Y and Y does not = X, X is related to X.
reflexive_relation(FunctorName) :-
    find_all_binary_relations(FunctorName,TermList),
    reflexive_terms(TermList,TermList).

reflexive_terms([],_).
reflexive_terms([Term|Terms],TermList) :-
    Term =.. [_|[Arg,Arg]],
    reflexive_terms(Terms,TermList).
reflexive_terms([Term|Terms],TermList) :-
    Term =.. [FunctorName|[Arg1,Arg2]],
    \+ (Arg1 = Arg2),
    ReflexiveTerm1 =.. [FunctorName|[Arg1,Arg1]],
    ReflexiveTerm2 =.. [FunctorName|[Arg2,Arg2]],
    member(ReflexiveTerm1,TermList),
    member(ReflexiveTerm2,TermList),
    reflexive_terms(Terms,TermList).

%Tests to ensure that X is related to Y and Y is related to X only when X = Y.
antisymmetric_relation(FunctorName) :-
    find_all_binary_relations(FunctorName,TermList),
    antisymmetric_terms(TermList,TermList).

antisymmetric_terms([],_).
antisymmetric_terms([Term|Terms],TermList) :-
    Term =.. [_|[Arg,Arg]],
    antisymmetric_terms(Terms,TermList).
antisymmetric_terms([Term|Terms],TermList) :-
    Term =.. [FunctorName|[Arg1,Arg2]],
    \+ (Arg1 = Arg2),
    SymmetricTerm =.. [FunctorName|[Arg2,Arg1]],
    \+ member(SymmetricTerm,TermList),
    antisymmetric_terms(Terms,TermList).