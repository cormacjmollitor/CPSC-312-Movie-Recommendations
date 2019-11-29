:- module(api, [call_person_search/2, call_keyword_search/2, call_discover/2, take_1/2]).

:- use_module(library(http/http_client)).
:- use_module(library(http/json)).
:- use_module(library(uri)).

api_key("af6b91ab4956fb3956740409f847efbb").
discover_url("https://api.themoviedb.org/3/discover/movie").
search_person_url("https://api.themoviedb.org/3/search/person").
search_keyword_url("https://api.themoviedb.org/3/search/keyword").

% Perform a search on people.
call_person_search(Name, Response) :-
	search_person_url(Url),
	call_search(Name, Url, Response).

% Perform a search on keywords.
call_keyword_search(Name, Response) :-
	search_keyword_url(Url),
	call_search(Name, Url, Response).

% Perform a /discover call to tmdb api.
% Assumes all params are valid /discover properties.
call_discover(Params, Response) :-
	discover_url(DiscoverUrl),
	generate_url(DiscoverUrl, Params, RequestUrl),
	make_request(RequestUrl, Response).

% Perform a search for Query at Url.
call_search(Query, Url, Response) :-
	uri_encoded(query_value, Query, EncodedQuery),
	generate_url(Url, [("query", EncodedQuery)], RequestUrl),
	make_request(RequestUrl, Response).

% Generates the correct URL with the provided query params.
generate_url(Url, Params, NewUrl) :-
	add_api_key(Url, NextUrl),
	add_query_params(NextUrl, Params, NewUrl).

% Adds api key as a query param to provided URL.
% Needs to be done first, as it has the "?" for starting query params hardcoded.
add_api_key(Url, NewUrl) :-
	api_key(ApiKey),
	string_concat("?api_key=", ApiKey, UrlEnd),
	string_concat(Url, UrlEnd, NewUrl).

% Adds query parameters to given url.
add_query_params(Url, [], Url).
add_query_params(Url, [(Key, Val)|Tail], NewUrl) :- 
	make_query_param(Key, Val, Param),
	string_concat(Url, Param, NextUrl),
	add_query_params(NextUrl, Tail, NewUrl).

% Transforms key, val => "&" + Key + "=" + Val
make_query_param(Key, Val, Param) :-
	string_concat("&", Key, Front),
	string_concat("=", Val, Back),
	string_concat(Front, Back, Param).

% Make the actual API call.
make_request(Url, Response) :-
	http_get(Url, JsonResponse, []),
	atom_json_dict(JsonResponse, Response, []).

% Returns the first element in a list
take_1([], []).
take_1([H|_], H).