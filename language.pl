:- module(language, [promptUser/0]).

:- use_module(api).
:- use_module(get_ids).
:- use_module(dictionary).

% Runs program
promptUser() :-
    write("Query for movies: "),
    flush_output(current_output),
    readln(Ln),
    question(Ln, Constraints),
    % write(Constraints),
    get_recommendation(Constraints, Recommendation),
    write_results(Recommendation),
    write(Constraints). % TEMP: To show that it's parsing correctly.

question(P0, Constraints) :-
    starter_phrase(P0, P1),
    movie_description(P1, _, _, Constraints, _).

% Print movie recommendation text, or a negative string if no movie was found.
write_results([]) :- 
    writeln("No movies match that description.").
write_results(Recommendation) :-
    writeln(Recommendation.original_title),
    writeln(Recommendation.release_date),
    writeln(Recommendation.overview).

% Fetches a movie recommendation when given a list of constraints
get_recommendation(Constraints, Recommendation) :-
    parse_query_params(Constraints, Params),
    merge_query_params(Params, QueryParams),
    call_discover(QueryParams, Response),
    take_1(Response.results, Recommendation).

% Maps predicate constraints onto their respective API key-value pair
parse_query_params([], []).
parse_query_params([Constraint|T], [Param|P]) :-
    parse_query_param(Constraint, Param),
    parse_query_params(T, P).

% Bi-directional conversion from predicate constraint to key-value pair
parse_query_param(rating(Rating, 'LessThan'), ('vote_average.lte', Rating)).
parse_query_param(rating(Rating, 'GreaterThan'), ('vote_average.gte', Rating)).
parse_query_param(date(Year, 'Year'), ('primary_release_year', Year)).
parse_query_param(date(Year, 'BeforeYear'), ('release_date.lte', Year)).
parse_query_param(date(Year, 'AfterYear'), ('release_date.gte', Year)).
parse_query_param(person(Name), (with_people, Id)) :-
    get_person_id(Name, Id).
parse_query_param(genre(Name), (with_genres, Id)) :-
    get_genre_id(Name, Id).
parse_query_param(plot_keyword(Keyword), (with_keywords, Id)) :-
    get_keyword_id(Keyword, Id).

% Merges key-value pairs with the same keys to (key,comma-seperatated vals) 
merge_query_params([], []).
merge_query_params([(Key,Val)|T], [(Key, ReducedVal)|ReducedParams]) :-
    get_all_params_of_type((Key,Val), T, Matches, Rest),
    reduce_values([(Key,Val)|Matches], "", ReducedVal),
    merge_query_params(Rest, ReducedParams).

% Reduces all values in key-value pairs to a single value
reduce_values([(_, Val)], OldString, NewString) :-
    atom_string(Val, String),
    string_concat(OldString, String, NewString).
reduce_values([(_,Val)|T], Initial, FinalString) :-
    atom_string(Val, ParamString),
    string_concat(ParamString, ",", ParamWithComma),
    string_concat(Initial, ParamWithComma, NewString),
    reduce_values(T, NewString, FinalString).

% Finds all key-value pairs with the same keys and puts them in the same list
% All non-matching key-value pairs go into the 4th parameter
get_all_params_of_type(_, [], [], []).
get_all_params_of_type((K,V), [(K,V2)|T], [(K,V2)|Matches], Rest) :-
    get_all_params_of_type((K,V), T, Matches, Rest).
get_all_params_of_type((K,V), [(K2,V2)|T], Matches, [(K2,V2)|Rest]) :-
    dif(K,K2),
    get_all_params_of_type((K,V), T, Matches, Rest).