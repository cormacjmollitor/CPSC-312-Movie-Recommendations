:- module(language, [promptUser/1]).
:- use_module(api).
:- use_module(dictionary).

% WIP: Still not hooked up to the API.
promptUser(Ans) :-
    write("Query for movies: "),
    flush_output(current_output),
    readln(Ln),
    question(Ln,End, C0, C1),
    write(C0). % To show that it's parsing correctly.

question(P0, P3, C0, C1) :-
    starter_phrase(P0, P1),
    movie_description(P1, P2, Params, C0, C1).