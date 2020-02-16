%%%-------------------------------------------------------------------
%%% @author mmooy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 2月 2020 17:37
%%%-------------------------------------------------------------------
-module(fibheap_test).
-author("mmooy").

-include_lib("eunit/include/eunit.hrl").
fib1_test() ->
    F1 = fibheap:new(),
    F2 = fibheap:insert(F1, {1, 1}),
    {{1, 1}, F3} = fibheap:pop(F2),
    F1 = F3.
fib2_test() ->
    F1 = fibheap:new(),
    F2 = fibheap:insert(F1, {1, 1}),
    F3 = fibheap:insert(F2, {2, 2}),
    {{1, 1}, F6} = fibheap:pop(F3),
    {{2, 2}, F7} = fibheap:pop(F6),
    F1 = F7.
fib3_test() ->
    F1 = fibheap:new(),
    F2 = fibheap:insert(F1, {1, 1}),
    F3 = fibheap:insert(F2, {2, 2}),
    F4 = fibheap:insert(F3, {3, 3}),
    {{1, 1}, F5} = fibheap:pop(F4),
    {{2, 2}, F6} = fibheap:pop(F5),
    {{3, 3}, F7} = fibheap:pop(F6),
    F1 = F7.
fib4_test() ->
    F0 = fibheap:new(),
    F1 = fibheap:insert(F0, {9, 9}),
    F2 = fibheap:insert(F1, {2, 2}),
    F3 = fibheap:insert(F2, {8, 8}),
    F4 = fibheap:insert(F3, {4, 4}),
    F5 = fibheap:insert(F4, {6, 6}),
    {{2, 2}, F6} = fibheap:pop(F5),
    {{4, 4}, F7} = fibheap:pop(F6),
    {{6, 6}, F8} = fibheap:pop(F7),
    {{8, 8}, F9} = fibheap:pop(F8),
    {{9, 9}, _F10} = fibheap:pop(F9),
    ok.
fib5_test() ->
    F0 = fibheap:new(),
    F1 = fibheap:insert(F0, {729, 729}),
    F2 = fibheap:insert(F1, {689, 689}),
    F3 = fibheap:insert(F2, {60, 60}),
    F4 = fibheap:insert(F3, {740, 740}),
    F5 = fibheap:insert(F4, {551, 551}),
    F6 = fibheap:insert(F5, {37, 37}),
    F7 = fibheap:insert(F6, {428, 428}),
    F8 = fibheap:insert(F7, {508, 508}),
    F9 = fibheap:insert(F8, {59, 59}),
    F10 = fibheap:insert(F9, {164, 164}),
    {{37, 37}, F11} = fibheap:pop(F10),
    {{59, 59}, F12} = fibheap:pop(F11),
    {{60, 60}, F13} = fibheap:pop(F12),
    {{164, 164}, F14} = fibheap:pop(F13),
    {{428, 428}, F15} = fibheap:pop(F14),
    {{508, 508}, F16} = fibheap:pop(F15),
    {{551, 551}, F17} = fibheap:pop(F16),
    {{689, 689}, F18} = fibheap:pop(F17),
    {{729, 729}, F19} = fibheap:pop(F18),
    {{740, 740}, _F20} = fibheap:pop(F19),
    ok.

fib6_test() ->
    F1 = fibheap:new(),
    F2 = fibheap:insert(F1, {1, 1}),
    F3 = fibheap:insert(F2, {1, 1}),
    {{1, 1}, F4} = fibheap:pop(F3),
    {{1, 1}, F5} = fibheap:pop(F4),
    F1 = F5.

fib7_test() ->
    F1 = fibheap:new(),
    lists:foldl(fun(_, {F2, List}) ->
        I = ceil(rand:uniform() * 1000),
        F3 = fibheap:insert(F2, {I, I}),
        Ls = [I | List],
        case rand:uniform() > 0.5 of
            true ->
                {{J, J}, _} = fibheap:get_min(F3),
                J = lists:min(Ls),
                {F3, Ls};
            _ ->
                {{J, J}, F4} = fibheap:pop(F3),
                J = lists:min(Ls),
                {F4, lists:delete(J, Ls)}
        end end, {F1, []}, lists:seq(1, 10000)).

