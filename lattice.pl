%Order theoretical tests
%Tests can be called on the name of a graph edge/dyadic relation, e.g. lattice(datatype).

%Reflexivity test
reflexive_relation(RelationName) :-
    find_all_binary_relations(RelationName,RelationTerms),
    gather_binary_args(RelationTerms,RelationArgs),
    reflexive_args(RelationName,RelationArgs,RelationTerms).

%Antisymmetry test
antisymmetric_relation(RelationName) :-
    find_all_binary_relations(RelationName,RelationTerms),
    antisymmetric_terms(RelationTerms,RelationTerms).

%Transitivity test
transitive_relation(RelationName) :-
    find_all_binary_relations(RelationName,RelationTerms),
    transitive_candidates(RelationTerms,PossiblyTransitivePairs),
    all_transitive(RelationTerms,PossiblyTransitivePairs).

%Poset test
poset(RelationName) :-
    find_all_binary_relations(RelationName,RelationTerms),
    gather_binary_args(RelationTerms,RelationArgs),
    reflexive_args(RelationName,RelationArgs,RelationTerms),
    antisymmetric_terms(RelationTerms,RelationTerms),
    transitive_candidates(RelationTerms,PossiblyTransitivePairs),
    all_transitive(RelationTerms,PossiblyTransitivePairs).

%Lattice test
lattice(PosetName) :-
    poset(PosetName),
    find_all_binary_relations(PosetName,PosetTerms),
    gather_binary_args(PosetTerms,PosetArgs),
    check_lattice(PosetName,PosetTerms,PosetArgs,PosetArgs).

%Order theoretical operators
%Operators can be called on the name of a poset and a subset of the poset, e.g. greatest_lower_bound(datatype,[float64,const],GreatestLowerBound).

%Greatest lower bound/infimum/maximal commmon subtype/meet operator
greatest_lower_bound(PosetName,Subset,GreatestLowerBound) :-
    poset(PosetName),
    set(Subset),
    find_all_binary_relations(PosetName,PosetTerms),
    gather_binary_args(PosetTerms,PosetArgs),
    set_inclusion(Subset,PosetArgs),
    find_all_lower_bounds(PosetName,Subset,PosetTerms,PosetArgs,LowerBounds),
    member(GreatestLowerBound,LowerBounds),
    check_greatest_lower_bound(PosetName,PosetTerms,LowerBounds,GreatestLowerBound).

%Least upper bound/supremum/minimal common supertype/join operator
least_upper_bound(PosetName,Subset,LeastUpperBound) :-
    poset(PosetName),
    set(Subset),
    find_all_binary_relations(PosetName,PosetTerms),
    gather_binary_args(PosetTerms,PosetArgs),
    set_inclusion(Subset,PosetArgs),
    find_all_upper_bounds(PosetName,Subset,PosetTerms,PosetArgs,UpperBounds),
    member(LeastUpperBound,UpperBounds),
    check_least_upper_bound(PosetName,PosetTerms,UpperBounds,LeastUpperBound).

%Auxiliary predicates

set([]).
set([Elem|Elems]) :-
    \+ member(Elem,Elems),
    set(Elems).

set_inclusion([],_).
set_inclusion([Term|Terms],RelationTerms) :-
    member(Term,RelationTerms),
    set_inclusion(Terms,RelationTerms).

find_all_binary_relations(RelationName,RelationTerms) :-
    Term =.. [RelationName|[_,_]],
    findall(Term,Term,RelationTerms).

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

reflexive_args(_,[],_).
reflexive_args(RelationName,[Arg|Args],RelationTerms) :-
    ReflexiveTerm =.. [RelationName|[Arg,Arg]],
    member(ReflexiveTerm,RelationTerms),
    reflexive_args(RelationName,Args,RelationTerms).

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

lower_bound(PosetName,Subset,PosetTerms,PosetArgs,LowerBound) :-
    member(LowerBound,PosetArgs),
    check_lower_bound(PosetName,PosetTerms,Subset,LowerBound).

check_lower_bound(_,_,[],LowerBound).
check_lower_bound(PosetName,PosetTerms,[Arg|Args],LowerBound) :-
    LowerBoundTemplate =.. [PosetName|[LowerBound,Arg]],
    member(LowerBoundTemplate,PosetTerms),
    check_lower_bound(PosetName,PosetTerms,Args,LowerBound).

find_all_lower_bounds(PosetName,Subset,PosetTerms,PosetArgs,LowerBounds) :-
    findall(LowerBound,lower_bound(PosetName,Subset,PosetTerms,PosetArgs,LowerBound),LowerBounds).

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

find_all_upper_bounds(PosetName,Subset,PosetTerms,PosetArgs,UpperBounds) :-
    findall(UpperBound,upper_bound(PosetName,Subset,PosetTerms,PosetArgs,UpperBound),UpperBounds).

check_least_upper_bound(_,_,[],LeastUpperBound).
check_least_upper_bound(PosetName,PosetTerms,[UpperBound|UpperBounds],LeastUpperBound) :-
    LeastUpperBoundTemplate =.. [PosetName|[LeastUpperBound,UpperBound]],
    member(LeastUpperBoundTemplate,PosetTerms),
    check_least_upper_bound(PosetName,PosetTerms,UpperBounds,LeastUpperBound).

check_lattice(_,_,[],_).
check_lattice(PosetName,PosetTerms,[Arg|Args],PosetArgs) :-
    check_arg_bounds(PosetName,PosetTerms,Arg,PosetArgs,PosetArgs),
    check_lattice(PosetName,PosetTerms,Args,PosetArgs).

check_arg_bounds(_,_,_,[],_).
check_arg_bounds(PosetName,PosetTerms,Arg,[Arg|Args],PosetArgs) :-
    check_arg_bounds(PosetName,PosetTerms,Arg,Args,PosetArgs).
check_arg_bounds(PosetName,PosetTerms,Arg1,[Arg2|Args],PosetArgs) :-
    \+ (Arg1 = Arg2),
    find_all_lower_bounds(PosetName,[Arg1,Arg2],PosetTerms,PosetArgs,LowerBounds),
    member(GreatestLowerBound,LowerBounds),
    check_greatest_lower_bound(PosetName,PosetTerms,LowerBounds,GreatestLowerBound),
    find_all_upper_bounds(PosetName,[Arg1,Arg2],PosetTerms,PosetArgs,UpperBounds),
    member(LeastUpperBound,UpperBounds),
    check_least_upper_bound(PosetName,PosetTerms,UpperBounds,LeastUpperBound),
    check_arg_bounds(PosetName,PosetTerms,Arg1,Args,PosetArgs).