p(St,conj(X,Y)) :- p(St,X),p(St,Y).
p(St,disj(X,Y)) :- p(St,X),p(St,Y).
p(St,disj(X,Y)) :- p(St,X),p___(St,Y).
p(St,disj(X,Y)) :- p_(St,X),p(St,Y).
p(St,neg(X)) :- p_____(St,X).
p(St,var(Z)) :- p_______(Z,St).
p_(St,conj(X__,Y_)) :- p(St,X__),p_(St,Y_).
p_(St,conj(X__,Y_)) :- p_(St,X__),p(St,Y_).
p_(St,conj(X__,Y_)) :- p_(St,X__),p_(St,Y_).
p_(St,disj(X__,Y_)) :- p_(St,X__),p_(St,Y_).
p_(St,neg(X__)) :- p(St,X__).
p_(St,var(Z)) :- p__(Z,St).
p__(o,[false|T]).
p__(s(N1),[H|T]) :- p__(N1,T).
p___(St,conj(X__,Y_)) :- p(St,X__),p___(St,Y_).
p___(St,conj(X__,Y_)) :- p___(St,X__),p(St,Y_).
p___(St,conj(X__,Y_)) :- p___(St,X__),p___(St,Y_).
p___(St,disj(X__,Y_)) :- p___(St,X__),p___(St,Y_).
p___(St,neg(X__)) :- p(St,X__).
p___(St,var(Z)) :- p____(Z,St).
p____(o,[false|T]).
p____(s(N1),[H|T]) :- p____(N1,T).
p_____(St,conj(X_,Y)) :- p(St,X_),p_____(St,Y).
p_____(St,conj(X_,Y)) :- p_____(St,X_),p(St,Y).
p_____(St,conj(X_,Y)) :- p_____(St,X_),p_____(St,Y).
p_____(St,disj(X_,Y)) :- p_____(St,X_),p_____(St,Y).
p_____(St,neg(X_)) :- p(St,X_).
p_____(St,var(Z)) :- p______(Z,St).
p______(o,[false|T]).
p______(s(N1),[H|T]) :- p______(N1,T).
p_______(o,[true|T]).
p_______(s(N1),[H|T]) :- p_______(N1,T).
<- p(St,Fm).