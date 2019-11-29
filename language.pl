:- module(language, [promptUser/0]).
:- use_module(api).
:- use_module(dictionary).

% WIP: Still not hooked up to the API.
promptUser() :-
    write("Query for movies: "),
    flush_output(current_output),
    readln(Ln),
    question(Ln, Constraints),
    write(Constraints). % To show that it's parsing correctly.

question(P0, Constraints) :-
    starter_phrase(P0, P1),
    movie_description(P1, _, _, Constraints, _).