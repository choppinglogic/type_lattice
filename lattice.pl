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
    gather_binary_args(RelationTerms,RelationArgs),
    reflexive_args(RelationName,RelationArgs,RelationTerms).

reflexive_args(_,[],_).
reflexive_args(RelationName,[Arg|Args],RelationTerms) :-
    ReflexiveTerm =.. [RelationName|[Arg,Arg]],
    member(ReflexiveTerm,RelationTerms),
    reflexive_args(RelationName,Args,RelationTerms).

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
    gather_binary_args(RelationTerms,RelationArgs),
    reflexive_args(RelationName,RelationArgs,RelationTerms),
    antisymmetric_terms(RelationTerms,RelationTerms),
    transitive_candidates(RelationTerms,PossiblyTransitivePairs),
    all_transitive(RelationTerms,PossiblyTransitivePairs).

%Lower and Upper Bounds

%This predicate, lower_bound/5, cannot be used as a full mathematical definition on its own - see the comment on find_all_lower_bounds/4.
lower_bound(PosetName,Subset,PosetTerms,PosetArgs,LowerBound) :-
    member(LowerBound,PosetArgs),
    check_lower_bound(PosetName,PosetTerms,Subset,LowerBound).

check_lower_bound(_,_,[],LowerBound).
check_lower_bound(PosetName,PosetTerms,[Arg|Args],LowerBound) :-
    LowerBoundTemplate =.. [PosetName|[LowerBound,Arg]],
    member(LowerBoundTemplate,PosetTerms),
    check_lower_bound(PosetName,PosetTerms,Args,LowerBound).

%Note the type-checking and set inclusion logic on the lower bound relation here, instead of lower_bound/5.
%This ensures that the type checks (poset,set), the parsing predicates, and the set inclusion checks only run once when finding multiple lower bounds.
find_all_lower_bounds(PosetName,Subset,PosetTerms,LowerBounds) :-
    poset(PosetName),
    find_all_binary_relations(PosetName,PosetTerms),
    gather_binary_args(PosetTerms,PosetArgs),
    set(Subset),
    set_inclusion(Subset,PosetArgs),
    findall(LowerBound,lower_bound(PosetName,Subset,PosetTerms,PosetArgs,LowerBound),LowerBounds).

greatest_lower_bound(PosetName,Subset,GreatestLowerBound) :-
    find_all_lower_bounds(PosetName,Subset,PosetTerms,LowerBounds),
    member(GreatestLowerBound,LowerBounds),
    check_greatest_lower_bound(PosetName,PosetTerms,LowerBounds,GreatestLowerBound).

check_greatest_lower_bound(_,_,[],GreatestLowerBound).
check_greatest_lower_bound(PosetName,PosetTerms,[LowerBound|LowerBounds],GreatestLowerBound) :-
    GreatestLowerBoundTemplate =.. [PosetName|[LowerBound,GreatestLowerBound]],
    member(GreatestLowerBoundTemplate,PosetTerms),
    check_greatest_lower_bound(PosetName,PosetTerms,LowerBounds,GreatestLowerBound).

upper_bound(PosetName,Subset,PosetTerms,PosetArgs,UpperBound) :-
    member(UpperBound,PosetArgs),
    check_upper_bound(PosetName,PosetTerms,Subset,UpperBound).

check_upper_bound(_,_,[],UpperBound).
check_upper_bound(PosetName,PosetTerms,[Arg|Args],UpperBound) :-
    UpperBoundTemplate =.. [PosetName|[Arg,UpperBound]],
    member(UpperBoundTemplate,PosetTerms),
    check_upper_bound(PosetName,PosetTerms,Args,UpperBound).

find_all_upper_bounds(PosetName,Subset,PosetTerms,UpperBounds) :-
    poset(PosetName),
    find_all_binary_relations(PosetName,PosetTerms),
    gather_binary_args(PosetTerms,PosetArgs),
    set(Subset),
    set_inclusion(Subset,PosetArgs),
    findall(UpperBound,upper_bound(PosetName,Subset,PosetTerms,PosetArgs,UpperBound),UpperBounds).

least_upper_bound(PosetName,Subset,LeastUpperBound) :-
    find_all_upper_bounds(PosetName,Subset,PosetTerms,UpperBounds),
    member(LeastUpperBound,UpperBounds),
    check_least_upper_bound(PosetName,PosetTerms,UpperBounds,LeastUpperBound).

check_least_upper_bound(_,_,[],LeastUpperBound).
check_least_upper_bound(PosetName,PosetTerms,[UpperBound|UpperBounds],LeastUpperBound) :-
    LeastUpperBoundTemplate =.. [PosetName|[LeastUpperBound,UpperBound]],
    member(LeastUpperBoundTemplate,PosetTerms),
    check_least_upper_bound(PosetName,PosetTerms,UpperBounds,LeastUpperBound).

lattice(PosetName) :-
    poset(PosetName),
    find_all_binary_relations(PosetName,PosetTerms),
    gather_binary_args(PosetTerms,PosetArgs),
    check_lattice(PosetName,PosetArgs,PosetArgs).

check_lattice(_,[],_).
check_lattice(PosetName,[Arg|Args],PosetArgs) :-
    check_arg_bounds(PosetName,Arg,PosetArgs),
    check_lattice(PosetName,Args,PosetArgs).

check_arg_bounds(_,_,[]).
check_arg_bounds(PosetName,Arg,[Arg|Args]) :-
    check_arg_bounds(PosetName,Arg,Args).
check_arg_bounds(PosetName,Arg1,[Arg2|Args]) :-
    \+ (Arg1 = Arg2),
    find_all_lower_bounds(PosetName,[Arg1,Arg2],PosetTerms,LowerBounds),
    member(GreatestLowerBound,LowerBounds),
    check_greatest_lower_bound(PosetName,PosetTerms,LowerBounds,GreatestLowerBound),
    find_all_upper_bounds(PosetName,[Arg1,Arg2],PosetTerms,UpperBounds),
    member(LeastUpperBound,UpperBounds),
    check_least_upper_bound(PosetName,PosetTerms,UpperBounds,LeastUpperBound),
    check_arg_bounds(PosetName,Arg1,Args).