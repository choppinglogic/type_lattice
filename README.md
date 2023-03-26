### lattice.pl
Toolkit for testing whether sets of dyadic Prolog relations - or lists of graph "triples" - are posets and if so, whether they are lattices, via order-theoretical property tests (reflexivity, antisymmetry, transitivity).

Also supports the greatest lower bound and least upper bound operators directly on knowledge graph triples, without using any numeric comparisons or calculations, or enforcing any naming conventions on the edges of knowledge graph triples. That said, the knowledge graph must obey the mathematical definition of a lattice to call the operators on it. For instance, if you have an edge relating node A to node B, you must also have an edge relating node A to node A (reflexivity), etc.

To use the module, consult lattice.pl and posets.pl at the Prolog REPL. The latter is a knowledge base that gives a few examples of posets. Each set of triples with a distinct name (e.g. "poset0," "poset1," "datatype," etc.) is a distinct poset. Some posets are also lattices, some are not.

Specify the functor name that identifies the poset when calling a property test, like so:

reflexive_relation(poset0).

poset(poset0).

lattice(poset3).

When calling an operator, supply the functor name that identifies the poset (e.g. "datatype") as well as the subset of elements of the poset that you are interested in (e.g. ⊤ and float64), like so:

greatest_lower_bound(datatype,['⊤',float64],X).

Note that the elements must be wrapped in a list to call the predicate. While typically you only want to supply two elements in the list to find their infimum or supremum (as with a standard "MaxValue or "MinValue" library function), you can also use the predicates to find the greatest lower bound or least upper bound of a poset subset with more or less than two elements:

least_upper_bound(poset2,[a,b,d],X).

least_upper_bound(poset2,[a],X).

### posets.pl
A sample knowledge base of posets. The first 3 are lattices, but the fourth is not. The "datatype" poset shows how to model a hiearchy of named types or concepts, such as the hierarchy of datatypes built into a programming language or hierarchies of categories in an upper ontology, as a lattice.