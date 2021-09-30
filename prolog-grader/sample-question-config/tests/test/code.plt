% plt files are recognized as external test files in Prolog.
% See: https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)#sec:3
%
% To run the file, use something like:
%
%  swipl -g 'load_test_files([]), run_tests' -t halt code.pl
%
% Note that -g simply runs the given goal. 
% -t gives the top-level interactive goal; so, -t halt prevents interactivity.
% load_test_files/1 automatically discovers code.plt from code.pl.

% The student's file should be a module and should export the appropriate predicate.
% This can be managed with the header!
% This file should use the module.

:- use_module(code, [my_reverse/2]).

type_tag(group, '=G=').
type_tag(plain, '=P=').
type_tag(required, '=R=').

test_decorated(Type, Name) :- 
  Type = group,
  type_tag(Type, Tag),
  Points >= 0,
  print(Tag, ' ', Name),

test_decorated(Type, Name, Points) :- 
  (Type = plain ; Type = required), type_tag(Type, Tag),
  integer(Points), Points >= 0,
  atomic_list_concat([Tag, ' ', Name, ' (', Points, ' points)'], TestID),
  todo.
  



revGroup('\n=G= Every test should be in a group.\n').
:- revGroup(Name), begin_tests(Name).


% A successful test.
test('=P= Simple reverse case, successful (1 points)') :- my_reverse([a,b], [b,a]).

% A failing test.
test('=P= Broken reverse test (1 points)') :- my_reverse([1,2,3], [1,2,3]).

:- revGroup(Name), end_tests(Name).
