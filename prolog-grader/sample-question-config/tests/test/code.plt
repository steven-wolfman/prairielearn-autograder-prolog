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

:- use_module(code).

revGroup('\n=G= Every test should be in a group.\n').
:- revGroup(Name), begin_tests(Name).

test('=P= Simple reverse case (1 points)') :- my_reverse([a,b], [b,a]).

:- revGroup(Name), end_tests(Name).