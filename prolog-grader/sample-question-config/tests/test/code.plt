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
%
% You can review the documentation for plunit at: https://www.swi-prolog.org/pldoc/doc_for?object=section(%27packages/plunit.html%27)
%
% You can review its source code at: https://github.com/SWI-Prolog/packages-plunit/blob/master/plunit.pl

% The student's file should be a module and should export the appropriate predicate.
% This can be managed with the header!
% This file should use the module.

:- use_module(code, [my_reverse/2]).

% NOT presently usable with forall option!
% That would be a great addition.


test_header_prefix('Begin PrairieLearn Prolog Test: ').
test_footer_prefix('End PrairieLearn Prolog Test: ').
test_header_prefix(Name, DecName) :-
  test_header_prefix(Header),
  atom_concat(Header, Name, DecName).
test_footer_prefix(Name, DecName) :-
  test_footer_prefix(Footer),
  atom_concat(Footer, Name, DecName).

test_group_header_prefix('Begin PrairieLearn Prolog Test Group: ').
test_group_footer_prefix('End PrairieLearn Prolog Test Group: ').
test_group_header_prefix(Name, DecName) :-
  test_group_header_prefix(Header),
  atom_concat(Header, Name, DecName).
test_group_footer_prefix(Name, DecName) :-
  test_group_footer_prefix(Footer),
  atom_concat(Footer, Name, DecName).

type_tag(group, '=G=').
type_tag(plain, '=P=').
type_tag(required, '=R=').

begin_test_decorated(Name) :- 
  type_tag(group, GTag), 
  atomic_list_concat([GTag, ' ', Name], FullName),
  test_group_header_prefix(FullName, HeaderName),
  test_group_footer_prefix(FullName, FooterName),
  begin_tests(Name, [setup((nl, write_term(HeaderName, []))),
                     cleanup((nl, write_term(FooterName, [])))
                    ]).

end_test_decorated(Name) :- 
  end_tests(Name).

% Update the given Options to include the given SetupBody at the start
% of the setup options, creating the setup option if needed.
update_setup(SetupBody, Options, NewOptions) :-
  selectchk(setup(OldGoal), Options, RemOptions),
  !, % setup options exist already
  NewOptions = [setup((SetupBody, OldGoal)) | RemOptions].
update_setup(SetupBody, Options, [setup(SetupBody) | Options]).
  

expand_term(Type, Name, Points, Options, Body, Result) :-
  (Type = plain ; Type = required), type_tag(Type, Tag),
  integer(Points), Points >= 0,
  atomic_list_concat([Tag, ' ', Name, ' (', Points, ' points)'], TestID),
  test_header_prefix(TestID, HeaderName),
  update_setup((nl, write_term(HeaderName, []), nl), Options, FullOptions),
  Result = (test(Name, FullOptions) :- Body).

term_expansion((test_decorated(Type, Name, Points) :- Body), Result) :-
  expand_term(Type, Name, Points, [], Body, Result).
term_expansion((test_decorated(Type, Name, Points, Options) :- Body), Result) :-
  expand_term(Type, Name, Points, Options, Body, Result).

%% I think this is what I want my test set to look like:

:- begin_test_decorated('Working Tests').
test_decorated(required, 'Empty list case', 1, []) :-
  my_reverse([], []).
test_decorated(plain, 'Some non-empty cases, right as output', 1, [all(X == [[b, a], [c, b, a], [[1, 2], [3, 4]]])]) :-
  ( my_reverse([a, b], X) ; 
    my_reverse([a, b, c], X) ;
    my_reverse([[3, 4], [1, 2]], X)).
% Runs forever, which is what "should" happen.
% test_decorated(plain, 'Some non-empty cases, left as output', 1, [all(X == [[b, a], [c, b, a], [[1, 2], [3, 4]]])]) :-
%   ( my_reverse(X, [a, b]) ; 
%     my_reverse(X, [a, b, c]) ;
%     my_reverse(X, [[3, 4], [1, 2]])
%   ).
test_decorated(plain, 'Some non-empty cases, mixed', 1, [all(X/Y == [a/b, a/b])]) :-
  ( my_reverse([X, b], [Y, a]) ; 
    my_reverse([b, _, X], [a, c, Y])
  ).
:- end_test_decorated('Working Tests').
:- begin_test_decorated('Broken Tests').
test_decorated(plain, 'Sample broken test', 2) :-
  my_reverse([a], []).
test_decorated(required, "Another broken test", 3) :-
  my_reverse([a], [a]), fail.
test_decorated(plain, 'Choice point', 3) :-
  my_reverse(_, []).
:- end_test_decorated('Broken Tests').

% revGroup('\n=G= Every test should be in a group.\n').
% :- revGroup(Name), begin_tests(Name).


% % A successful test.
% test('=P= Simple reverse case, successful (1 points)') :- my_reverse([a,b], [b,a]).

% % A failing test.
% test('=P= Broken reverse test (1 points)') :- my_reverse([1,2,3], [1,2,3]).

% :- revGroup(Name), end_tests(Name).
