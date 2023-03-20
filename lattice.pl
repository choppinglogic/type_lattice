%Sample knowledge base. "covers(X,Y)" means Y covers X.
:- dynamic covers/2.
covers(p,q).
covers(q,r).
covers(p,r).
covers(p,s).

%Utility predicates

%Finds all binary relations with a given functor name in the knowledge base.
find_all_binary_relations(FunctorName,TermList) :-
    Term =.. [FunctorName|[_,_]],
    findall(Term,Term,TermList).

%Order-theoretical property tests
%Call a property test by specifying a functor name for binary terms in your knowledge base, e.g. "strict_partial_order(covers)"

%Reflexivity test
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

%Irreflexivity test
irreflexive_relation(FunctorName) :-
    find_all_binary_relations(FunctorName,TermList),
    irreflexive_terms(TermList,TermList).

irreflexive_terms([],_).
irreflexive_terms([Term|Terms],TermList) :-
    Term =.. [_|[Arg1,Arg2]],
    \+ (Arg1 = Arg2),
    irreflexive_terms(Terms,TermList).

%Antisymmetry test
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

%Transitivity test
transitive_relation(FunctorName) :-
    find_all_binary_relations(FunctorName,TermList),
    transitive_candidates(TermList,PossiblyTransitivePairs),
    all_transitive(TermList,PossiblyTransitivePairs).

possibly_transitive_pair(TermList,Term1,Term2) :-
    member(Term1,TermList),
    member(Term2,TermList),
    Term1 =.. [FunctorName|[Arg1,Arg2]],
    Term2 =.. [FunctorName|[Arg2,Arg3]],
    \+ (Arg1 = Arg2),
    \+ (Arg2 = Arg3).

transitive_terms(Term1,Term2,Term3) :-
    Term1 =.. [FunctorName|[Arg1,Arg2]],
    Term2 =.. [FunctorName|[Arg2,Arg3]],
    Term3 =.. [FunctorName|[Arg1,Arg3]],
    \+ (Arg1 = Arg2),
    \+ (Arg2 = Arg3).

transitive_candidates(TermList,PossiblyTransitivePairs) :-
    findall([Term1,Term2],possibly_transitive_pair(TermList,Term1,Term2),PossiblyTransitivePairs).

all_transitive(_,[]).
all_transitive(TermList,[[Term1,Term2]|PossiblyTransitivePairs]) :-
    member(Term3,TermList),
    transitive_terms(Term1,Term2,Term3),
    all_transitive(TermList,PossiblyTransitivePairs).

%Strict partial order test
strict_partial_order(FunctorName) :-
    find_all_binary_relations(FunctorName,TermList),
    irreflexive_terms(TermList,TermList),
    transitive_candidates(TermList,PossiblyTransitivePairs),
    all_transitive(TermList,PossiblyTransitivePairs).