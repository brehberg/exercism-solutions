%! flatten_list(+Xs, -XsFlattened)
%
% The flatten_list/2 predicate succeeds with XsFlattened
% as a single flattened list with all values except nil.
flatten_list(Xs, XsFlattened) :-
  flatten(Xs, XsFlat),
  subtract(XsFlat, [nil], XsFlattened).
