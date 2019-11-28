:- module(dictionary, [starter_phrase/2, noun_phrase/5, modifying_phrase/5]).
:- use_module(get_ids).

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
    celebrity(P2, P3, Entity, C2, C3),
    movie_genre(P3, P4, Entity, C3, C4),
    noun(P4, P5, Entity, C4, C5),
    modifying_phrase(P5, P6, Entity, C5, C6).

% e.g. a crime movie from 2010 starring Ellen Page and directed by Christopher Nolan
% ['a', 'crime', 'movie', 'from', '2010', 'starring', 'Ellen', 'Page', 'and', 'directed', 'by', 'Christopher', 'Nolan']
% noun_phrase(P0, P6, Entity, C0, C6) :-
%     det(P0, P1, Entity, C0, C1),
%     movie_genre(P3, P4, Entity, C3, C4),
%     noun(P4, P5, Entity, C4, C5),
%     release_date(P1, P2, Entity, C1, C2),
%     celebrity(P2, P3, Entity, C2, C3),
%     modifying_phrase(P5, P6, Entity, C5, C6).

% e.g. a 2010 movie by Christopher Nolan starring Ellen Page
% noun_phrase(P0, P6, Entity, C0, C6) :-
%     det(P0, P1, Entity, C0, C1),
%     release_date(P1, P2, Entity, C1, C2),
%     celebrity(P2, P3, Entity, C2, C3),
%     movie_genre(P3, P4, Entity, C3, C4),
%     noun(P4, P5, Entity, C4, C5),
%     modifying_phrase(P5, P6, Entity, C5, C6).

det(['a' | P], P, _, C, C).
det(P, P, _, C, C).

% Query is looking for release date before specified year
release_date(['from', 'before', Num|P], P, _, [date(Num, 'BeforeYear')|C], C) :- atom_number(Num, _).
release_date(['made', 'before', Num|P], P, _, [date(Num, 'BeforeYear')|C], C) :- atom_number(Num, _).
release_date(['released', 'before', Num|P], P, _, [date(Num, 'BeforeYear')|C], C) :- atom_number(Num, _).
release_date(['before', Num|P], P, _, [date(Num, 'BeforeYear')|C], C) :- atom_number(Num, _).

% Query is looking for release date after specified year
release_date(['after', Num|P], P, _, [date(Num, 'AfterYear')|C], C) :- atom_number(Num, _).

% Query is looking for release date IN specified year
release_date(['from', Num|P], P, _, [date(Num, 'Year')|C], C) :- atom_number(Num, _).
release_date([Num|P], P, _, [date(Num, 'Year')|C], C) :- atom_number(Num, _).

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
    get_genre_id(Genre, Id).
movie_genre(Tail, Tail, _, C, C).

noun(['movie' | P], P, _, C, C).
noun(['film' | P], P, _, C, C).

% modifying_phrase can be 'with ___', 'starring ___', 'by ___', 'directed by ___'...
% or 'released in 2006', 'from the 1990s'...
% modifying_phrase(['starring'|P], P, _, celebrity(P, Rest, _, C0, C1), [C0|C], C).
% modifying_phrase(['from' |P0], P6, _, C0, C6) :-
%     noun_phrase(P0, P6, _, C0, C6).
% modifying_phrase(['starring' |P0], P6, _, C0, C6) :-
%     noun_phrase(P0, P6, _, C0, C6).
% modifying_phrase(['and' |P0], P6, _, C0, C6) :-
%     noun_phrase(P0, P6, _, C0, C6).
modifying_phrase(P, P, _, C, C).

% reln(['starring' | L],L,O1,O2,[borders(O1,O2)|C],C).
% reln(['directed', 'by' | L],L,O1,O2, [capital(O2,O1)|C],C).
% reln(['with' | L],L,O1,O2, [borders(O1,O2)|C],C).

% Checks if first letter of an Atom is capitalized
is_capitalized(Atom) :-
    atom_string(Atom, String),
    get_string_code(1,String,First),
    code_type(First,upper).

date(Year, BeforeOrAfter).
person(Name).
genre(Genre).