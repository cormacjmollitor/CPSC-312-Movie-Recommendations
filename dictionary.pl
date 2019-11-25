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
release_date(P, P, _, C, C).

% check for capitalization to determine if director_name?
director_name(P, P, _, C, C).

% genre is from a hardcoded list of TMDb's recognized genres
genre(P, P, _, C, C).

noun([movie | P], P).
noun([film | P], P).

% modifying_phrase can be 'with ___', 'starring ___', 'by ___', 'directed by ___'...
% or 'released in 2006', 'from the 1990s'...
modifying_phrase(P, P, _, C, C).