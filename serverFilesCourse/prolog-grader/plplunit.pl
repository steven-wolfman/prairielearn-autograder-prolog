% The PrairieLearn Prolog Unit Testing library wraps the plunit library.
%:- module(plplunit, [begin_test_decorated/1, end_test_decorated/1]).

%:- use_module(library(plunit), [begin_tests/2, end_tests/1]).

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
