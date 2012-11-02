-module(bloom_filter).

-export([new/2, add_element/2, is_element/2, fpr/2]).
-import(math, [log/1, pow/2, exp/1]).

-define(SALT, ?MODULE).

-type bloom_filter() :: {binary(), pos_integer(), pos_integer()}.

-spec new(pos_integer(), float()) -> bloom_filter().
new(_, ErrorRate) when ErrorRate =< 0; ErrorRate > 1.0 ->
    error(badarg);
new(Size, ErrorRate) ->
    {M, K} = layout(Size, ErrorRate),
    {hipe_bifs:bitarray(M, false), M, K}.

-spec add_element(term(), bloom_filter()) -> bloom_filter().
add_element(Key, BloomFilter) ->
    lists:foldl(fun (Idx, BF) -> set_bit(Idx, BF) end,
                BloomFilter,
                indices(Key, BloomFilter)).

-spec is_element(term(), bloom_filter()) -> boolean().
is_element(Key, BloomFilter) ->
    lists:all(fun (Idx) -> is_bit(Idx, BloomFilter) end,
              indices(Key, BloomFilter)).

-spec fpr(pos_integer(), bloom_filter()) -> float().
fpr(N, {_Array, M, K}) ->
    pow(1 - exp(-K * (N + 0.5) / (M - 1)), K).

set_bit(Idx, {Array, M, K}) when Idx < M ->
    {hipe_bifs:bitarray_update(Array, Idx, true), M, K}.

is_bit(Idx, {Array, M, _K}) when Idx < M ->
    hipe_bifs:bitarray_sub(Array, Idx).

layout(N, P) ->
    M = trunc(-(N * log(P))/pow(log(2),2) + 1),
    K = trunc(M * log(2) / N + 1),
    {M, K}.

indices(Key, {_Array, M, K}) ->
    lists:map(fun (I) -> double_hash(Key, I) rem M end,
              lists:seq(0, K - 1)).

double_hash(Key, I) -> hash1(Key) + I * hash2(Key).
hash1(Key) -> erlang:phash2(Key).
hash2(Key) -> erlang:phash2({?SALT, Key}).
