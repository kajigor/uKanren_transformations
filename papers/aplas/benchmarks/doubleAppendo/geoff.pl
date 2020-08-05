p(X__,Y,Z,R) :- p_(X__,Y,T,Z,R).
p_([H|T_],Y,[H|Ty],Z,[H|Ty_]) :- p_(T_,Y,Ty,Z,Ty_).
p_([],T,T,Z,R) :- p__(T,Z,R).
p__([H|T_],Z,[H|Ty]) :- p__(T_,Z,Ty).
p__([],R,R).
<- p(X__,Y,Z,R).