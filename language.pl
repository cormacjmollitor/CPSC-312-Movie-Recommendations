:- use_module(api).
:- use_module(dictionary).

question(P1, P3) :-
    starter_phrase(P0, P1),
    noun_phrase(P1, P2, Params),
    modifying_phrase(P2, P3, Params, NewParams).