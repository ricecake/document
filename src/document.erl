-module(document).

-record(ap_state, {set, sim, res, ava}).

%% API exports
-export([
	shingle/2,
	semi_match/2,
	lcs/2,
	levenshtein/2,
	cluster/1,
	pairs/1,
	tokenize/1
]).

%%====================================================================
%% API functions
%%====================================================================

shingle(Document, Size) ->
	Tokens = tokenize(Document),
	do_shingle(Tokens, Size, []).

semi_match(DocumentA, DocumentB) ->
	lcs(tokenize(DocumentA), tokenize(DocumentB)).

lcs(A, B) ->
	{LCS, _Cache} = get_lcs(A, B, [], #{}),
	lists:reverse(LCS).

levenshtein(A, B) ->
	{Dist, _Cache} = get_levenshtein(A, B, #{}),
	Dist.

cluster(Set) ->
	CleanSet = lists:usort(Set),
	Pairs = [ {A, B} || A <- CleanSet, B <- CleanSet],
	%Similarity     = maps:from_list([ {{A, B}, -1*levenshtein(A, B)} || {A, B} <- Pairs]),
	Similarity     = maps:from_list([ {{A, B}, -1*(levenshtein(A, B)/length(lcs(A,B)))} || {A, B} <- Pairs]),
	AvgSim = lists:sum([ V||{_, V} <- maps:to_list(Similarity)]) / maps:size(Similarity),
	SelfSim = maps:from_list([ {{Item, Item}, AvgSim} || Item <- Set ]),
	Responsibility = maps:from_list([ {{A, B}, 0} || {A, B} <- Pairs]),
	Availability   = maps:from_list([ {{A, B}, 0} || {A, B} <- Pairs]),
	#ap_state{ res=Res, ava=Ava} = do_ap_round_many({ap_state, CleanSet, maps:merge(Similarity, SelfSim), Responsibility, Availability}, 500),
	lists:foldl(fun({Ex, Ex}, Acc)-> Acc; ({Ex, I}, Acc)-> Acc#{ Ex => [ I |maps:get(Ex, Acc, [])] } end, #{}, [ {Ex, I} || {_, {I, Ex}} <- [ lists:max(lists:sort([{maps:get({I, K}, Res)+maps:get({I, K}, Ava), {I, K}} ||  K <- CleanSet])) ||  I <- CleanSet]]).

%%====================================================================
%% Internal functions
%%====================================================================

dampen(Key, Val, OldMap) -> {Key, 0.5*maps:get(Key, OldMap) + (1-0.5)*Val}.


do_ap_round_many(S, N) when N =< 0 -> S;
do_ap_round_many(S, N) ->
	NewS = do_ap_round(S),
	do_ap_round_many(NewS, N-1).

do_ap_round(State) ->
	PostResState = do_update_res(State),
	do_update_ava(PostResState).

do_update_res(#ap_state{set=Set, sim=Sim, ava=Ava, res=Res} = State) ->
	State#ap_state{ res = maps:from_list([ dampen({I, K}, maps:get({I, K}, Sim) - lists:max([maps:get({I, Kp}, Ava)+maps:get({I,Kp}, Sim) || Kp <- Set, Kp =/= K  ]), Res) || K <- Set, I <- Set ]) }.

do_update_ava(#ap_state{set=Set, res=Res, ava=Ava} = State) ->
	NewAvaA = maps:from_list([ dampen({I,K}, lists:min([0, maps:get({K,K}, Res) + lists:sum([ lists:max([0, maps:get({Ip, K}, Res)]) || Ip <- Set, Ip =/= K, Ip =/= I])]), Ava) || I <- Set, K <- Set, I =/= K]),
	NewAvaB = maps:from_list([ dampen({K, K}, lists:sum([lists:max([0, maps:get({Ip, K}, Res)])|| Ip <- Set, Ip =/= K]), Ava)|| K <- Set]),
	State#ap_state{ ava=maps:merge(NewAvaA, NewAvaB) }.

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
	do_combine(Head, Rest, List, [{Head, Item} |Acc]).

tokenize(A) ->
	case re:run(A, <<"[\\[\\]()]|[\\w.\\-]+|\\S+">>, [{capture, all, binary}, global, unicode]) of
		{match, ListA} -> lists:flatten(ListA);
		_ -> binary:split(A, <<" ">>, [global])
	end.
	

maybe_wildcard([wildcard |_]=Acc) -> Acc;
maybe_wildcard(Acc) -> [wildcard |Acc].
