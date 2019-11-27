:- use_module(api).

%take_n([], _, []).
%take_n(_, 0, []).
%take_n([H|T], N, [H|R]) :-
%	N2 is N-1,
%	take_n(T, N2, R).

take_1([], []).
take_1([H|_], H).

get_person_id(Name, Id) :-
	call_person_search(Name, Response),
	take_1(Response.results, PersonResult),
	Id is PersonResult.id.



