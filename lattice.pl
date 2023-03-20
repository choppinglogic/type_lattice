%Sample knowledge base.
:- dynamic divides/2.
divides(1,1).
divides(1,2).
divides(2,2).
divides(1,3).
divides(3,3).
divides(1,4).
divides(2,4).
divides(4,4).
divides(1,5).
divides(5,5).
divides(1,6).
divides(2,6).
divides(3,6).
divides(6,6).

%Utility predicates

%Finds all binary relations with a given functor name in the knowledge base.
find_all_binary_relations(RelationName,RelationTerms) :-
    Term =.. [RelationName|[_,_]],
    findall(Term,Term,RelationTerms).

%Finds all unique arguments in a list of binary relations.
gather_binary_args(RelationTerms,RelationArgs) :-
    gather_binary_args(RelationTerms,[],RelationArgs).

gather_binary_args([],RelationArgs,RelationArgs).
gather_binary_args([Term|Terms],UniqueArgs,RelationArgs) :-
    Term =.. [_|[Arg,Arg]],
    \+ member(Arg,UniqueArgs),
    UniqueArgs1 = [Arg|UniqueArgs],
    gather_binary_args(Terms,UniqueArgs1,RelationArgs).
gather_binary_args([Term|Terms],UniqueArgs,RelationArgs) :-
    Term =.. [_|[Arg,Arg]],
    member(Arg,UniqueArgs),
    gather_binary_args(Terms,UniqueArgs,RelationArgs).
gather_binary_args([Term|Terms],UniqueArgs,RelationArgs) :-
    Term =.. [_|[Arg1,Arg2]],
    \+ (Arg1 = Arg2),
    \+ member(Arg1,UniqueArgs),
    \+ member(Arg2,UniqueArgs),
    UniqueArgs1 = [Arg1,Arg2|UniqueArgs],
    gather_binary_args(Terms,UniqueArgs1,RelationArgs).
gather_binary_args([Term|Terms],UniqueArgs,RelationArgs) :-
    Term =.. [_|[Arg1,Arg2]],
    \+ (Arg1 = Arg2),
    \+ member(Arg1,UniqueArgs),
    member(Arg2,UniqueArgs),
    UniqueArgs1 = [Arg1|UniqueArgs],
    gather_binary_args(Terms,UniqueArgs1,RelationArgs).
gather_binary_args([Term|Terms],UniqueArgs,RelationArgs) :-
    Term =.. [_|[Arg1,Arg2]],
    \+ (Arg1 = Arg2),
    member(Arg1,UniqueArgs),
    \+ member(Arg2,UniqueArgs),
    UniqueArgs1 = [Arg2|UniqueArgs],
    gather_binary_args(Terms,UniqueArgs1,RelationArgs).
gather_binary_args([Term|Terms],UniqueArgs,RelationArgs) :-
    Term =.. [_|[Arg1,Arg2]],
    \+ (Arg1 = Arg2),
    member(Arg1,UniqueArgs),
    member(Arg2,UniqueArgs),
    gather_binary_args(Terms,UniqueArgs,RelationArgs).

%Set theoretical tests
set([]).
set([Elem|Elems]) :-
    \+ member(Elem,Elems),
    set(Elems).

set_inclusion([],_).
set_inclusion([Term|Terms],RelationTerms) :-
    member(Term,RelationTerms),
    set_inclusion(Terms,RelationTerms).

%Order theoretical property tests
%Call a property test by specifying a functor name for binary terms in your knowledge base, e.g. "strict_partial_order(covers)"

%Reflexivity test
reflexive_relation(RelationName) :-
    find_all_binary_relations(RelationName,RelationTerms),
    reflexive_terms(RelationTerms,RelationTerms).

reflexive_terms([],_).
reflexive_terms([Term|Terms],RelationTerms) :-
    Term =.. [_|[Arg,Arg]],
    reflexive_terms(Terms,RelationTerms).
reflexive_terms([Term|Terms],RelationTerms) :-
    Term =.. [RelationName|[Arg1,Arg2]],
    \+ (Arg1 = Arg2),
    ReflexiveTerm1 =.. [RelationName|[Arg1,Arg1]],
    ReflexiveTerm2 =.. [RelationName|[Arg2,Arg2]],
    member(ReflexiveTerm1,RelationTerms),
    member(ReflexiveTerm2,RelationTerms),
    reflexive_terms(Terms,RelationTerms).

%Irreflexivity test
irreflexive_relation(RelationName) :-
    find_all_binary_relations(RelationName,RelationTerms),
    irreflexive_terms(RelationTerms,RelationTerms).

irreflexive_terms([],_).
irreflexive_terms([Term|Terms],RelationTerms) :-
    Term =.. [_|[Arg1,Arg2]],
    \+ (Arg1 = Arg2),
    irreflexive_terms(Terms,RelationTerms).

%Antisymmetry test
antisymmetric_relation(RelationName) :-
    find_all_binary_relations(RelationName,RelationTerms),
    antisymmetric_terms(RelationTerms,RelationTerms).

antisymmetric_terms([],_).
antisymmetric_terms([Term|Terms],RelationTerms) :-
    Term =.. [_|[Arg,Arg]],
    antisymmetric_terms(Terms,RelationTerms).
antisymmetric_terms([Term|Terms],RelationTerms) :-
    Term =.. [RelationName|[Arg1,Arg2]],
    \+ (Arg1 = Arg2),
    SymmetricTerm =.. [RelationName|[Arg2,Arg1]],
    \+ member(SymmetricTerm,RelationTerms),
    antisymmetric_terms(Terms,RelationTerms).

%Transitivity test
transitive_relation(RelationName) :-
    find_all_binary_relations(RelationName,RelationTerms),
    transitive_candidates(RelationTerms,PossiblyTransitivePairs),
    all_transitive(RelationTerms,PossiblyTransitivePairs).

possibly_transitive_pair(RelationTerms,Term1,Term2) :-
    member(Term1,RelationTerms),
    member(Term2,RelationTerms),
    Term1 =.. [RelationName|[Arg1,Arg2]],
    Term2 =.. [RelationName|[Arg2,Arg3]],
    \+ (Arg1 = Arg2),
    \+ (Arg2 = Arg3),
    \+ (Arg1 = Arg3).

transitive_terms(Term1,Term2,Term3) :-
    Term1 =.. [RelationName|[Arg1,Arg2]],
    Term2 =.. [RelationName|[Arg2,Arg3]],
    Term3 =.. [RelationName|[Arg1,Arg3]],
    \+ (Arg1 = Arg2),
    \+ (Arg2 = Arg3),
    \+ (Arg1 = Arg3).

transitive_candidates(RelationTerms,PossiblyTransitivePairs) :-
    findall([Term1,Term2],possibly_transitive_pair(RelationTerms,Term1,Term2),PossiblyTransitivePairs).

all_transitive(_,[]).
all_transitive(RelationTerms,[[Term1,Term2]|PossiblyTransitivePairs]) :-
    member(Term3,RelationTerms),
    transitive_terms(Term1,Term2,Term3),
    all_transitive(RelationTerms,PossiblyTransitivePairs).

%Strict partial order test
poset(RelationName) :-
    find_all_binary_relations(RelationName,RelationTerms),
    reflexive_terms(RelationTerms,RelationTerms),
    antisymmetric_terms(RelationTerms,RelationTerms),
    transitive_candidates(RelationTerms,PossiblyTransitivePairs),
    all_transitive(RelationTerms,PossiblyTransitivePairs).

%Lower and Upper Bounds
lower_bound(RelationName,Subset,LowerBound) :-
    poset(RelationName),
    find_all_binary_relations(RelationName,PosetTerms),
    gather_binary_args(PosetTerms,PosetArgs),
    set(Subset),
    set_inclusion(Subset,PosetArgs),
    member(LowerBound,PosetArgs),
    check_lower_bound(RelationName,PosetTerms,Subset,LowerBound).

check_lower_bound(_,_,[],LowerBound).
check_lower_bound(RelationName,PosetTerms,[Arg|Args],LowerBound) :-
    LowerBoundTemplate =.. [RelationName|[LowerBound,Arg]],
    member(LowerBoundTemplate,PosetTerms),
    check_lower_bound(RelationName,PosetTerms,Args,LowerBound).