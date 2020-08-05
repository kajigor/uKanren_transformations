p(Subst,constr(N,A),var_(V)) :- p____(V,Subst,Subst,N,A,T).
p(Subst,constr(N1,A1),constr(N2,A2)) :- p_(N1,N2),p__(Subst,A1,A2).
p(Subst,var_(V),constr(N,A)) :- p___(V,Subst,Subst,T,N,A).
p(Subst,var_(V1),var_(V2)) :- p_____(V1,Subst,Subst,T11,V2).
p(Subst,var_(V1),var_(V2)) :- p______(Subst,V1,V2).
p_(s(X___),s(Y)) :- p_(X___,Y).
p_(z,z).
p__(Subst,[X___|Xs],[Y|Ys]) :- p(Subst,X___,Y),p__(Subst,Xs,Ys).
p__(Subst,[],[]).
p___(s(N_),[X___|Xs],X__,T,N,A) :- p___(N_,Xs,X__,T,N,A).
p___(z,[some(T)|Xs],X__,T,N,A) :- p(X__,T,constr(N,A)).
p____(s(N_),[X___|Xs],X__,N,A,T) :- p____(N_,Xs,X__,N,A,T).
p____(z,[some(T)|Xs],X__,N,A,T) :- p(X__,constr(N,A),T).
p_____(s(N),[X___|Xs],X__,T11,V2) :- p_____(N,Xs,X__,T11,V2).
p_____(z,[some(T11)|Xs],X__,T11,V2) :- p(X__,T11,var_(V2)).
p______([X|Xs],s(N),s(N_)) :- p______(Xs,N,N_).
p______([none|Xs],z,z).
p______([],V1,V2) :- p_______(V1,V2).
p_______(s(X),s(Y)) :- p_______(X,Y).
p_______(z,z).
<- p(Subst,Fm,Fm1).