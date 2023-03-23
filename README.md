# conceptual_graph
Prototype implementations of Sowa's conceptual graph modeling approach, which is detailed in:

Sowa, John F. *Conceptual Structures: Information Processing in Mind and Machine*. The Systems Programming Series. Reading, MA: Addison-Wesley Publishing Company, 1984.

### cgraph_dcg.pl
The Prolog definite clause grammar module implements the formal grammar detailed in pp. 395-396.

### lattice.pl
Toolkit for testing whether SETS of dyadic Prolog relations - or lists of graph "triples" - are lattices.

This is done through order theoretical property tests (reflexivity, antisymmetry, transitivity), greatest lower bound and least upper bound operators, and poset and lattice type checks.

To use the module, consult lattice.pl and posets.pl at the Prolog REPL. The latter is a sample knowledge base that gives a few examples of posets.

Specify the FUNCTOR NAME that identifies the poset when calling a property test, like so:

reflexive_relation(poset0).

poset(poset0).

lattice(poset3).

When calling an operator, supply the FUNCTOR NAME that identifies the poset as well as the subset of elements of the poset that you are interested in, like so:
greatest_lower_bound(poset2,[b,c,d],X).

### posets.pl
A sample knowledge base of 4 example posets. The first 3 are lattices, but the fourth is not.
