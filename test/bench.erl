-module(bench).

-export([run/0, do_run/3]).

% # erl -pa ../ebin
% > c(bench), bench:run().
%
% Adds 20 million keys to a bloom filter for 50 million keys
%

-define(N, 50000000).
-define(N_INSERTS, 20000000).
-define(FPR, 0.01).
-define(REPETITIONS, 1).

run() ->
    BF = bloom_filter:new(?N, ?FPR),
    Self = self(),
    spawn(fun() ->
                  {Time, R} = timer:tc(?MODULE, do_run,
                                       [BF, ?N_INSERTS, ?REPETITIONS]),

                  SharedMem = byte_size(element(1, R)) / 1024 / 1024,
                  ProcMem = element(2, erlang:process_info(self(), memory))
                                / 1024 / 1024,
                  {garbage_collection, GC} =
                      erlang:process_info(self(), garbage_collection),
                  GCs = proplists:get_value(minor_gcs, GC),
                  Self ! {stats, {Time * math:pow(10, -6), ProcMem, SharedMem, GCs}}
          end),

    receive
        {stats, {Time, ProcMem, SharedMem, GCs}} ->
            io:format("Time: ~p s Mem (proc): ~p Mb Mem (shared): ~p Mb GC: ~p~n",
                      [Time, ProcMem, SharedMem, GCs])
    end.

do_run(Initial, Elements, 1) ->
    add_elements(Initial, Elements);

do_run(Initial, Elements, N) ->
    _BloomFilter = add_elements(Initial, Elements),
    do_run(Initial, Elements, N - 1).

add_elements(BloomFilter, 0) ->
    BloomFilter;

add_elements(BloomFilter, N) ->
    add_elements(bloom_filter:add_element(random:uniform(), BloomFilter), N-1).

