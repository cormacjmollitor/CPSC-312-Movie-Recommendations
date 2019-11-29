:- module(get_ids, [get_person_id/2, get_keyword_id/2, get_genre_id/2]).

:- use_module(api).

% Get the ID for a specified person.
get_person_id(Name, Id) :-
	call_person_search(Name, Response),
	take_1(Response.results, PersonResult),
	is_dict(PersonResult),
	Id is PersonResult.id.

% Get the ID for a specified keyword.
get_keyword_id(Keyword, Id) :-
	call_keyword_search(Keyword, Response),
	take_1(Response.results, KeywordResult),
	is_dict(KeywordResult),
	Id is KeywordResult.id.

get_genre_id('Action', 28).
get_genre_id('action', 28).
get_genre_id('Adventure', 12).
get_genre_id('adventure', 12).
get_genre_id('Animation', 16).
get_genre_id('animation', 16).
get_genre_id('Comedy', 25).
get_genre_id('comedy', 25).
get_genre_id('Crime', 80).
get_genre_id('crime', 80).
get_genre_id('Documentary', 99).
get_genre_id('documentary', 99).
get_genre_id('Drama', 18).
get_genre_id('drama', 18).
get_genre_id('Family', 10751).
get_genre_id('family', 10751).
get_genre_id('Fantasy', 14).
get_genre_id('fantasy', 14).
get_genre_id('History', 36).
get_genre_id('history', 36).
get_genre_id('Horror', 27).
get_genre_id('horror', 27).
get_genre_id('Music', 10402).
get_genre_id('music', 10402).
get_genre_id('Musical', 10402).
get_genre_id('musical', 10402).
get_genre_id('Mystery', 9648).
get_genre_id('mystery', 9648).
get_genre_id('Romance', 10749).
get_genre_id('romance', 10749).
get_genre_id('Science Fiction', 878).
get_genre_id('science fiction', 878).
get_genre_id('scifi', 878).
get_genre_id('SciFi', 878).
get_genre_id('TV Movie', 10770).
get_genre_id('tv movie', 10770).
get_genre_id('Thriller', 53).
get_genre_id('thriller', 53).
get_genre_id('War', 10752).
get_genre_id('war', 10752).
get_genre_id('Western', 37).
get_genre_id('western', 37).
