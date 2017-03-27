-module(document).

%% API exports
-export([
	shingle/2,
	semi_match/2,
	lcs2/2
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

%%====================================================================
%% Internal functions
%%====================================================================

do_shingle([], _, Acc) -> lists:reverse(Acc);
do_shingle(List, N, Acc) when length(List) < N -> do_shingle([], N, Acc);
do_shingle([_ | Rest] = List, N, Acc) ->
	{SubList, _} = lists:split(N, List),
	do_shingle(Rest, N, [SubList | Acc]).


lcs(A, B) ->
	get_lcs(A, B, [], #{}).

get_lcs(A, B, _Acc, #{ {A, B} := LCS } = Cache ) -> {LCS, Cache}.

compute_lcs(A, B, Acc, Cache) when length(A) == 0 orelse length(B) == 0->
	LCS = lists:reverse(Acc),
	{LCS, Cache#{ {A, B} => LCS} };
compute_lcs([Token |ATail], [Token |BTail], Acc, Cache) ->
	get_lcs(ATail, BTail, [Token |Acc], Cache);
compute_lcs([_AToken |ATail]=A, [_BToken |BTail]=B, Acc, Cache) ->
	{LCSA, CacheA} = get_lcs(A, BTail, Acc, Cache),
	{LCSB, CacheB} = get_lcs(ATail, B, Acc, CacheA),
	{lists:max([LCSA, LCSB]), CacheB}.

	

maybe_wildcard([wildcard |_]=Acc) -> Acc;
maybe_wildcard(Acc) -> [wildcard |Acc].
