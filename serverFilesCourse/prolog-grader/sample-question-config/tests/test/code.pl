% Use this for a sample solution, which you can then use for development testing, 
% to ensure your test suite passes a reference implementation and that you're satisfied
% with test output.

:- module(code, [my_reverse/2]).


%! my_reverse(+L1: list, L2: list)
%
%  True if L2 is L1 backward.
%
my_reverse(Xs, Ys) :- rev_helper(Xs, [], Ys).

rev_helper([], Acc, Acc).
rev_helper([X|Xs], Acc, Out) :- rev_helper(Xs, [X|Acc], Out).
