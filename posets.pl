%Example posets for use as tests with lattice.pl
%poset0, poset1, and poset2 are lattices; poset3 is not.
poset0(a,a).

poset1(a,a).
poset1(a,b).
poset1(b,b).

poset2(a,a).
poset2(a,b).
poset2(a,c).
poset2(a,d).
poset2(a,e).
poset2(b,b).
poset2(b,d).
poset2(b,e).
poset2(c,c).
poset2(c,e).
poset2(d,d).
poset2(d,e).
poset2(e,e).

poset3(a,a).
poset3(a,b).
poset3(a,c).
poset3(a,d).
poset3(a,e).
poset3(a,f).
poset3(b,b).
poset3(b,d).
poset3(b,e).
poset3(b,f).
poset3(c,c).
poset3(c,d).
poset3(c,e).
poset3(c,f).
poset3(d,d).
poset3(d,f).
poset3(e,e).
poset3(e,f).
poset3(f,f).

poset4(null,null).
poset4(null,[1]).
poset4(null,[2]).
poset4(null,[3]).
poset4(null,[1,2]).
poset4(null,[2,3]).
poset4(null,[1,3]).
poset4(null,[1,2,3]).
poset4([1],[1]).
poset4([1],[1,2]).
poset4([1],[1,3]).
poset4([1],[1,2,3]).
poset4([2],[2]).
poset4([2],[1,2]).
poset4([2],[2,3]).
poset4([2],[1,2,3]).
poset4([3],[3]).
poset4([3],[1,3]).
poset4([3],[2,3]).
poset4([3],[1,2,3]).
poset4([1,2],[1,2]).
poset4([1,2],[1,2,3]).
poset4([1,3],[1,3]).
poset4([1,3],[1,2,3]).
poset4([2,3],[2,3]).
poset4([2,3],[1,2,3]).
poset4([1,2,3],[1,2,3]).

pentagon(0,0).
pentagon(0,a).
pentagon(0,b).
pentagon(0,c).
pentagon(0,1).
pentagon(a,a).
pentagon(a,1).
pentagon(b,b).
pentagon(b,1).
pentagon(c,c).
pentagon(c,a).
pentagon(c,1).
pentagon(1,1).

