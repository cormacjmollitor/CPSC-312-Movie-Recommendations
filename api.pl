:- use_module(library(http/http_client)).
:- use_module(library(http/json)).

% Example: call_search("marvel", Result).
% Example function (with helper), gets 1st movie response.
% example(Result) :-
%	 call_search("marvel", Response),
%	 take_1(Response.results, Result).
% take_1([H|_], [H]). 

api_key("af6b91ab4956fb3956740409f847efbb").
discover_url("https://api.themoviedb.org/3/discover/movie").
search_url("https://api.themoviedb.org/3/search/movie").

% Perform a search on Query string in tmdb search api.
call_search(Query, Response) :-
	search_url(SearchUrl),
	generate_url(SearchUrl, [("query", Query)], RequestUrl),
	make_request(RequestUrl, Response).

% Perform a /discover call to tmdb api.
% Assumes all params are valid /discover properties.
call_discover(Params, Response) :-
	discover_url(DiscoverUrl),
	generate_url(DiscoverUrl, Params, RequestUrl),
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

