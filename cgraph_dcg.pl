cgraph --> concept,cgraph_terminal.
cgraph --> concept,rlink,cgraph_terminal.
cgraph --> relation,conlink,cgraph_terminal.

rlink --> arc,relation.
rlink --> arc,relation,conlink.
rlink --> hyphen,rlist,comma.

conlink --> arc,concept.
conlink --> arc,concept,rlink.
conlink --> hyphen,conlist,comma.

rlist --> newline,relation.
rlist --> newline,relation,conlink.
rlist --> newline,relation,rlist.
rlist --> newline,relation,conlink,rlist.

conlist --> newline,arc,concept.
conlist --> newline,arc,concept,rlink.
conlist --> newline,arc,concept,conlist.
conlist --> newline,arc,concept,rlink,conlist.

concept --> lbrack,typefield,rbrack.
concept --> lbrack,typefield,colon,reffield,rbrack.

relation --> lparen,typelabel,rparen.

arc --> larrow.
arc --> rarrow.
arc --> num_seq,larrow.
arc --> num_seq,rarrow.

num_seq --> num_elem.
num_seq --> num_elem,num_seq.

cgraph_terminal --> [X],{member(X,['.',';'])}.
colon --> [':'].
comma --> [','].
hyphen --> ['-'].
lbrack --> ['['].
rbrack --> [']'].
lparen --> ['('].
rparen --> [')'].
larrow --> ['←'].
rarrow --> ['→'].
newline --> ['\n'].
num_elem --> [X],{member(X,[0,1,2,3,4,5,6,7,8,9])}.