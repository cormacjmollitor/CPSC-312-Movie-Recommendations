:- module(dictionary, [starter_phrase/2, noun_phrase/5]).
starter_phrase(['What', 'is' | P], P).
starter_phrase(['What', '\'', 's' | T], T).
starter_phrase([Imperative | T], T) :-
    imperative(Imperative).
starter_phrase([Imperative, Asker | T], T) :-
    imperative(Imperative).
    asker(Asker).
starter_phrase(['I', 'want' | P], P).
starter_phrase(['I', 'want', 'to', 'watch' | P], P).

imperative('Give').
imperative('Suggest').
imperative('Recommend').

asker('me').
asker('us').

% e.g. a 2010 Christopher Nolan crime movie starring Ellen Page
noun_phrase(P0, P6, Entity, C0, C6) :-
    det(P0, P1, Entity, C0, C1),
    release_date(P1, P2, Entity, C1, C2),
    director_name(P2, P3, Entity, C2, C3),
    genre(P3, P4, Entity, C3, C4),
    noun(P4, P5, Entity, C4, C5),
    modifying_phrase(P5, P6, Entity, C5, C6).

det([a | P], P, _, C, C).
det(P, P, _, C, C).

% release_date - check for year (number) or years (number + 's')
% Why is the _ an underscore and not Entity?
% release_date(P, P, _, C, C).
release_date([Num|P], P, _, [number_string(_,Num)|C],C).

% check for capitalization to determine if director_name?
% Why is the _ an underscore and not Entity?
% director_name(P, P, _, C, C).
director_name([First, Last|P], P, _, [is_capitalized(First), is_capitalized(Last)|C], C).

% genre is from a hardcoded list of TMDb's recognized genres
% genre(P, P, _, C, C).
genre([Genre|P], P, _, [get_genre_id(Genre)|C], C).

noun(['movie' | P], P, _, C, C).
noun(['film' | P], P, _, C, C).

% modifying_phrase can be 'with ___', 'starring ___', 'by ___', 'directed by ___'...
% or 'released in 2006', 'from the 1990s'...
modifying_phrase(P, P, _, C, C).

% Checks if first letter of a string is capitalized
is_capitalized(String) :-
    get_string_code(1,String,First),code_type(First,upper).