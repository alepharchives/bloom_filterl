-module(bloom_filter).

-export([new/2, add_element/2, is_element/2]).
-import(math, [log/1, pow/2]).

-define(SALT, ?MODULE).

new(Size, ErrorRate) ->
    {M, K} = layout(Size, ErrorRate),
    {hipe_bifs:bitarray(M, false), M, K}.

add_element(Key, BloomFilter) ->
    lists:foldl(fun (Idx, BF) -> set_bit(Idx, BF) end,
                BloomFilter,
                indices(Key, BloomFilter)).

is_element(Key, BloomFilter) ->
    lists:all(fun (Idx) -> is_bit(Idx, BloomFilter) end,
              indices(Key, BloomFilter)).

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
