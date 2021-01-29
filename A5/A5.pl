% Adarsha Kanel
% 30049820
% refrence used for q2: henrique tut 04: December 07, 2020
%						henrique tut 04: December, 02, 2020
%						https://www.swi-prolog.org/pldoc/man?predicate=is_list/1
%						https://www.swi-prolog.org/pldoc/man?section=lists

class(a).
class(b).
class(c).
extends(a,b).
extends(b,c).
extends(d,e).
extends(e,f).
interface(d).
interface(e).
interface(f).
implements(c,d).
%1a
%sub class takes input X and Y and check if X and Y are both classes such that X extends to Y
subclass(X,Y) :- class(X), class(Y), extends(X,Y).
subclass(X,Y) :- class(X), class(Y), extends(X,A), subclass(A,Y).

%1b	
%superinterface takes input Y and X and  succeeds iff Y is an interface, X is a class, and either X or a superclass of X implements an interface Z, such that either Z is Y, or Z is related to Y via the transitive closure of extends
superinterface(Y,X):- interface(Y), class(X), implements(X,Y).
superinterface(Y,X):- interface(Y), class(X), extends(A,Y), superinterface(A,X).
superinterface(Y,X):- interface(Y), class(X), extends(X,A), superinterface(Y,A).

%2
%elemof takes in arguments graph [(C,_)|Rest] or [(_,C)|Rest] and cover [C|Cs] and then recursively finds all the vertex cover of the graph ( a variation of henriques get_unique_relationship example shown in December 07,2020)
elemof([],[]).
elemof([G|Rest],[C|Cs]):- (G=(C,_); G=(_,C)), elemof(Rest, Cs).

%vertex_cover takes in a Graph G and a Cover C, and either find if the cover is a valid vertex cover for G or find all possible vertex cover C for G
%append graph with iself and return it as K, so that all possible elements appear in the vertex cover ( as just sending graph as argument to elemof gives maximum of minimum vertex cover)
vertex_cover(Graph, C):-  is_list(C),append(Graph,Graph,K), sort(C,P), elemof(K,Z), sort(Z,P).
vertex_cover(Graph, C):- not(is_list(C)),append(Graph,Graph,K), elemof(K,P), sort(P,C).