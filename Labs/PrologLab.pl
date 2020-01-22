allzero([]).
allzero([0|T]) :- allzero(T).

sum(leaf(N), N).
sum(node(L, R), S) :- sum(L, SL), sum(R, SR), S is SL + SR.

sublist([], _ ).
sublist([X|XS], [X|XSS]) :- sublist(XS, XSS).
sublist([X|XS], [_|XSS]) :- sublist([X|XS], XSS).