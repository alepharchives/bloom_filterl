-module(bloom_filter_tests).
-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

proper_test() ->
    ?assert(proper:quickcheck(prop_add_element())).

prop_add_element() ->
    ?FORALL({Element, Previous, Size}, {term(), list(term()), pos_integer()},
            begin
                Filter = lists:foldl(fun (T, F) ->
                                             bloom_filter:add_element(T, F)
                                     end,
                                     bloom_filter:new(Size, random:uniform()),
                                     [Element | Previous]),
                bloom_filter:is_element(Element, Filter)
            end).

error_rate_test() ->
    Previous = ordsets:from_list([random:uniform() || _ <- lists:seq(1, 1000)]),
    ErrorRate = 0.0001,
    Size = trunc(1.05 * length(Previous)), %% Simulate bloom filter at 95% capacity
    Filter = lists:foldl(fun (T, F) ->
                                 bloom_filter:add_element(T, F)
                         end,
                         bloom_filter:new(Size, ErrorRate),
                         Previous),
    Other = [random:uniform() || _ <- lists:seq(1, 100000)],
    Errors = lists:sum(lists:map(
                fun (E) ->
                        case (not ordsets:is_element(E, Previous)) andalso
                                bloom_filter:is_element(E, Filter) of
                            true  -> 1;
                            false -> 0
                        end
                end, Other)),
    ?assert(Errors / length(Other) =< ErrorRate).
