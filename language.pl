:- module(language, [promptUser/1]).
:- use_module(api).
:- use_module(dictionary).

% WIP: Untested and still not hooked up to the API.
promptUser(Ans) :-
    write("Query for movies: "),
    flush_output(current_output),
    readln(Ln),
    question(Ln,End),
    member(End,[[],['?'],['.']]).

question(P1, P3) :-
    starter_phrase(P0, P1),
    noun_phrase(P1, P2, Params),
    modifying_phrase(P2, P3, Params, NewParams).