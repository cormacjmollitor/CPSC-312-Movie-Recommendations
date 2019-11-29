:- module(dictionary, [starter_phrase/2, movie_description/5]).

:- use_module(get_ids).

starter_phrase(['What', 'is' | P], P).
starter_phrase(['What', '\'', 's' | T], T).
starter_phrase([Imperative | T], T) :-
    imperative(Imperative).
starter_phrase([Imperative, Asker | T], T) :-
    imperative(Imperative),
    asker(Asker).
starter_phrase(['I', 'want' | P], P).
starter_phrase(['I', 'want', 'to', 'watch' | P], P).

temporal_phrase(['from', 'before' | P], P).
temporal_phrase(['made', 'before' | P], P).
temporal_phrase(['released', 'before' | P], P).
temporal_phrase(['before' | P], P).

temporal_phrase(['after' | P], P).
temporal_phrase(['released', 'after' | P], P).

temporal_phrase(['released', 'in' | P], P).
temporal_phrase(['from' | P], P).

imperative('Give').
imperative('give').
imperative('Suggest').
imperative('suggest').
imperative('Recommend').
imperative('recommend').

asker('me').
asker('us').

% e.g. a 2010 Christopher Nolan crime movie starring Ellen Page
movie_description(P0, P6, Entity, C0, C6) :-
    det(P0, P1, Entity, C0, C1),
    release_date(P1, P2, Entity, C1, C2),
    celebrity(P2, P3, Entity, C2, C3),
    movie_genre(P3, P4, Entity, C3, C4),
    noun(P4, P5, Entity, C4, C5),
    modifying_phrase(P5, P6, Entity, C5, C6).

det(['a' | P], P, _, C, C).
det(P, P, _, C, C).

% Query is looking for release date IN specified year
release_date([Num|P], P, _, [date(Num, 'Year')|C], C) :- number(Num).
release_date(['from', Num|P], P, _, [date(Num, 'Year')|C], C) :- number(Num).
release_date(['in', Num|P], P, _, [date(Num, 'Year')|C], C) :- number(Num).

% Query is looking for release date after specified year
release_date(['after', Num|P], P, _, [date(Num, 'AfterYear')|C], C) :- number(Num).

% Query is looking for release date before specified year
release_date(['before', Num|P], P, _, [date(Num, 'BeforeYear')|C], C) :- number(Num).
release_date(['from', 'before', Num|P], P, _, [date(Num, 'BeforeYear')|C], C) :- number(Num).
release_date(['made', 'before', Num|P], P, _, [date(Num, 'BeforeYear')|C], C) :- number(Num).
release_date(['released', 'before', Num|P], P, _, [date(Num, 'BeforeYear')|C], C) :- number(Num).

% Query does not specify a year
release_date(P, P, _, C, C).

% check for capitalization to determine if this is a person.
celebrity([First, Middle, Last|P], P, _, [person(FullName)|C], C) :- 
    atom_concat(First, " ", FirstName),
    atom_concat(Middle, " ", MiddleName),
    atom_concat(FirstName, MiddleName, FrontName),
    atom_concat(FrontName, Last, FullName),
    is_capitalized(First),
    is_capitalized(Middle),
    is_capitalized(Last).
celebrity([First, Last|P], P, _, [person(FullName)|C], C) :-
    atom_concat(First, " ", FirstName),
    atom_concat(FirstName, Last, FullName),
    is_capitalized(First),
    is_capitalized(Last).
celebrity([First|P], P, _, [person(First)|C], C) :- is_capitalized(First).
celebrity(P, P, _, C, C). % Case where there isn't a name

% genre is from a hardcoded list of TMDb's recognized genres
movie_genre([Genre|P], P, _, [genre(Genre)|C], C) :- 
    get_genre_id(Genre, _).
movie_genre(Tail, Tail, _, C, C).

noun(['movie' | P], P, _, C, C).
noun(['film' | P], P, _, C, C).

% modifying_phrase can be 'with ___', 'starring ___', 'by ___', 'directed by ___'...
% or 'released in 2006', 'from the 1990s'...
% Handles punctuation at end of sentence.
modifying_phrase([], _, _, C, C).
modifying_phrase([?], _, _, C, C).
modifying_phrase([.], _, _, C, C).
modifying_phrase([!], _, _, C, C).
% Modifying phrases indicating an actor or director
modifying_phrase(['with'|P], P, _, C0, C2) :- 
    celebrity(P, T, _, C0, C1),
    modifying_phrase(T, _, _, C1, C2).
modifying_phrase(['starring'|P], P, _, C0, C2) :- 
    celebrity(P, T, _, C0, C1),
    modifying_phrase(T, _, _, C1, C2).
modifying_phrase(['directed', 'by'|P], P, _, C0, C2) :- 
    celebrity(P, T, _, C0, C1),
    modifying_phrase(T, _, _, C1, C2).

% Modifying phrases about release dates
modifying_phrase(P, T, _, C0, C2) :- 
    release_date(P, T, _, C0, C1),
    modifying_phrase(T, _, _, C1, C2).

modifying_phrase(P, P, _, C, C).

% Checks if first letter of an Atom is capitalized
is_capitalized(Atom) :-
    atom_string(Atom, String),
    get_string_code(1,String,First),
    code_type(First,upper).