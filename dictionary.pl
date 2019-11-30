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
movie_description(P0, P8, Entity, C0, C8) :-
    det(P0, P1, Entity, C0, C1),
    quality_adj(P1, P2, Entity, C1, C2),
    release_date(P2, P3, Entity, C2, C3),
    celebrity(P3, P4, Entity, C3, C4),
    keyword(P4, P5, Entity, C4, C5),
    movie_genre(P5, P6, Entity, C5, C6),
    noun(P6, P7, Entity, C6, C7),
    modifying_phrase(P7, P8, Entity, C7, C8).

det(['a' | P], P, _, C, C).
det(['an' | P], P, _, C, C).
det(['the' | P], P, _, C, C).
det(P, P, _, C, C).

quality_adj(['bad' | P], P, _, [rating(5, 'LessThan')|C], C).
quality_adj(['mediocre' | P], P, _, [rating(5, 'GreaterThan'), rating(6, 'LessThan')|C], C).
quality_adj(['okay' | P], P, _, [rating(6, 'GreaterThan'), rating(7, 'LessThan')|C], C).
quality_adj(['good' | P], P, _, [rating(7, 'GreaterThan') |C], C).
quality_adj(['great' | P], P, _, [rating(8, 'GreaterThan') |C], C).
quality_adj(['amazing' | P], P, _, [rating(9, 'GreaterThan') |C], C).
quality_adj(P, P, _, C, C).

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

% Query does not specify a year but specifies a timeframe
release_date(['new'|P], P, _, [date(2019, 'Year')|C], C).
release_date(['recent'|P], P, _, [date(2016, 'AfterYear')|C], C).
release_date(['old'|P], P, _, [date(2010, 'BeforeYear')|C], C).

% Query does not specify a timeframe nor a year
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

% keyword is whatever is not any of the other things...
keyword([Keyword|P], P, _, [plot_keyword(Keyword)|C], C) :-
    \+ det([Keyword|P], P, _, C, C),
    \+ quality_adj([Keyword|P], P, _, C, C),
    \+ release_date([Keyword|P], P, _, C, C),
    \+ celebrity([Keyword|P], P, _, C, C),
    \+ movie_genre([Keyword|P], P, _,  C, C),
    \+ noun([Keyword|P], P, _, _, _),
    \+ modifying_phrase([Keyword|P], P, _, C, C),
    atom_length(Keyword, Length),
    Length > 2.
keyword(P, P, _, C, C).

% genre is from a hardcoded list of TMDb's recognized genres
movie_genre([Genre|P], P, _, [genre(Genre)|C], C) :- 
    get_genre_id(Genre, _).
movie_genre(Tail, Tail, _, C, C).

noun(['movie' | P], P, _, C, C).
noun(['film' | P], P, _, C, C).

% Modifying phrases can specify requirements for any movie aspect (i.e. add any constraint)
% that is listed under movie_description
% e.g. quality, actors/directors, release dates, etc.
% Also handles punctuation at end of sentence.
modifying_phrase([], _, _, C, C).
modifying_phrase([?], _, _, C, C).
modifying_phrase([.], _, _, C, C).
modifying_phrase([!], _, _, C, C).

% Modifying phrases about quality (rating) of movie
modifying_phrase(['that', 'is' | P], P, _, C0, C2) :-
    quality_adj(P, T, _, C0, C1),
    modifying_phrase(T, _, _, C1, C2).
modifying_phrase(['that\'s' | P], P, _, C0, C2) :-
    quality_adj(P, T, _, C0, C1),
    modifying_phrase(T, _, _, C1, C2).

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
modifying_phrase(['by'|P], P, _, C0, C2) :- 
    celebrity(P, T, _, C0, C1),
    modifying_phrase(T, _, _, C1, C2).

% Modifying phrases about keyword
modifying_phrase(['about'|P], P, _, C0, C2) :- 
    writeln(P),
    keyword(P, T, _, C0, C1),
    modifying_phrase(T, _, _, C1, C2).

% Modifying phrases about release dates
modifying_phrase(['that', 'is' | P], P, _, C0, C2) :-
    release_date(P, T, _, C0, C1),
    modifying_phrase(T, _, _, C1, C2).
modifying_phrase(['that\'s' | P], P, _, C0, C2) :-
    release_date(P, T, _, C0, C1),
    modifying_phrase(T, _, _, C1, C2).
modifying_phrase(P, T, _, C0, C2) :- 
    release_date(P, T, _, C0, C1),
    modifying_phrase(T, _, _, C1, C2).

% No modifying phrase
modifying_phrase(P, P, _, C, C).

% Checks if first letter of an Atom is capitalized
is_capitalized(Atom) :-
    atom_string(Atom, String),
    get_string_code(1,String,First),
    code_type(First,upper).