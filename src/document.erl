-module(document).

%% API exports
-export([
	shingle/2,
	semi_match/2,
	lcs/2,
	levenshtein/2,
	cluster/1,
	pairs/1
]).

%%====================================================================
%% API functions
%%====================================================================

shingle(Document, Size) ->
	Tokens = binary:split(Document, <<" ">>, [global]),
	do_shingle(Tokens, Size, []).

semi_match(DocumentA, DocumentB) ->
	{match, ListA} = re:run(DocumentA, <<"[\\[\\]()]|[\\w.\\-]+|\\S+">>, [{capture, all, binary}, global]),
	{match, ListB} = re:run(DocumentB, <<"[\\[\\]()]|[\\w.\\-]+|\\S+">>, [{capture, all, binary}, global]),
	TokenA = lists:flatten(ListA),
	TokenB = lists:flatten(ListB),
	lcs(TokenA, TokenB).

lcs(A, B) ->
	{LCS, _Cache} = get_lcs(A, B, [], #{}),
	lists:reverse(LCS).

levenshtein(A, B) ->
	{Dist, _Cache} = get_levenshtein(A, B, #{}),
	Dist.

cluster(Set) ->
	[ {A, B, levenshtein(A, B)} || {A, B} <- pairs(Set)].

%%====================================================================
%% Internal functions
%%====================================================================

do_shingle([], _, Acc) -> lists:reverse(Acc);
do_shingle(List, N, Acc) when length(List) < N -> do_shingle([], N, Acc);
do_shingle([_ | Rest] = List, N, Acc) ->
	{SubList, _} = lists:split(N, List),
	do_shingle(Rest, N, [SubList | Acc]).


get_lcs(A, B, Acc, Cache) ->
	case maps:find({A, B, Acc}, Cache) of
		{ok, LCS} -> {LCS, Cache};
		error     ->
			{NewLCS, NewCache} = compute_lcs(A, B, Acc, Cache),
			{NewLCS, NewCache#{ {A, B, Acc} => NewLCS }}
	end.

compute_lcs(A, B, Acc, Cache) when length(A) == 0 orelse length(B) == 0 ->
	{Acc, Cache};
compute_lcs([Token |ATail], [Token |BTail], Acc, Cache) ->
	get_lcs(ATail, BTail, [Token |Acc], Cache);
compute_lcs([_AToken |ATail]=A, [_BToken |BTail]=B, AccIn, Cache) ->
	Acc = maybe_wildcard(AccIn),
	{LCSA, CacheA} = get_lcs(A, BTail, Acc, Cache),
	{LCSB, CacheB} = get_lcs(ATail, B, Acc, CacheA),
	LCS = case length(LCSA) > length(LCSB) of
		true  -> LCSA;
		false -> LCSB
	end,
	{LCS, CacheB}.


get_levenshtein(A, B, Cache) ->
	case maps:find({A, B}, Cache) of
		{ok, Dist} -> {Dist, Cache};
		error     ->
			{NewDist, NewCache} = compute_levenshtein(A, B, Cache),
			{NewDist, NewCache#{ {A, B} => NewDist }}
	end.

compute_levenshtein(A, B, Cache) when length(A) == 0 orelse length(B) == 0 ->
	{lists:max([length(A), length(B)]), Cache};
compute_levenshtein([Token|ATail], [Token|BTail], Cache) ->
	get_levenshtein(ATail, BTail, Cache);
compute_levenshtein([_AToken |ATail]=A, [_BToken |BTail]=B, Cache) ->
	{Dist1,Cache1} = get_levenshtein(A,     BTail, Cache),
	{Dist2,Cache2} = get_levenshtein(ATail, B,     Cache1),
	{Dist3,Cache3} = get_levenshtein(ATail, BTail, Cache2),
	{1+lists:min([Dist1, Dist2, Dist3]), Cache3}.


pairs([Head |List]) ->
	do_combine(Head, List, List, []).

do_combine(_Head, [], [], Acc) -> Acc;
do_combine(_Head, [], [NewHead |NewList], Acc) ->
	do_combine(NewHead, NewList, NewList, Acc);
do_combine(Head, [Item |Rest], List, Acc) ->
	Tuple = case Head > Item of
		true  -> {Head, Item};
		false -> {Item, Head}
	end,
	do_combine(Head, Rest, List, [Tuple |Acc]).

maybe_wildcard([wildcard |_]=Acc) -> Acc;
maybe_wildcard(Acc) -> [wildcard |Acc].
