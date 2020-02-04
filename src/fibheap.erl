%%%-------------------------------------------------------------------
%%% @author mmooy
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. 2月 2020 12:11
%%%-------------------------------------------------------------------
-module(fibheap).
-author("mmooyyii").

-export([new/0, insert/2, get_min/1, pop/1, delete/2, union/2, size/1]).

-define(Undefined, undefined).
-type fibHeap() :: tuple().
-record(fib_heap, {depth, min, memory}).
-record(node, {left, right, child, parent, key, val}).

%% API

-spec new() -> fibHeap().
new() ->
    #fib_heap{depth = 0, memory = maps:new()}.

-spec insert(fibHeap(), term()) -> fibHeap().
insert(#fib_heap{memory = Memory, min = ?Undefined}, {K, V}) ->
    Node = #node{key = K, val = V},
    #fib_heap{min = K, depth = 0, memory = Memory#{K => Node}};

insert(F = #fib_heap{memory = Memory, min = Min, depth = Depth}, {K, V}) ->
    case maps:is_key(K, Memory) of
        true ->
            update(F, {K, V});
        _ ->
            NewMin = case K < Min of true -> Min; _ -> K end,
            Root = #node{left = RootLeft} = maps:get(Memory, F),
            Node = #node{left = RootLeft, right = Min, val = V},
            #fib_heap{memory = Memory#{K => Node, Min => Root#node{left = K}}, min = NewMin, depth = Depth}
    end.

-spec get_min(fibHeap()) -> term().
get_min(#fib_heap{memory = Memory, min = K}) ->
    case K of
        ?Undefined -> {error, empty};
        _ -> #node{val = Val} = maps:get(K, Memory), {K, Val}
    end.

-spec pop(fibHeap()) -> term().
pop(_) ->
    ok.

-spec delete(fibHeap(), term()) -> term().
delete(_, _) ->
    ok.

-spec union(fibHeap(), fibHeap()) -> fibHeap().
union(
    #fib_heap{memory = Memory1, min = Min1, depth = Depth1},
    #fib_heap{memory = Memory2, min = Min2, depth = Depth2}) ->
    Root1 = #node{key = Key1} = get_left(Memory1, Min1),
    Root2 = #node{key = Key2} = get_left(Memory2, Min2),
    M1 = Memory1#{Key1=> Root1#node{left = Key2}},
    M2 = Memory2#{Key2=> Root2#node{left = Key1}},
    #fib_heap{memory = maps:merge(M1, M2), min = min(Min1, Min2), depth = max(Depth1, Depth2)}.

-spec size(fibHeap()) -> integer().
size(#fib_heap{memory = Memory}) ->
    maps:size(Memory).

update(#fib_heap{memory = Memory, min = Min, depth = Depth}, {K, V}) ->
    case maps:is_key(Memory, K) of
        true ->
            Node = maps:get(Memory, K),
            #fib_heap{memory = Memory#{K=>Node#node{val = V}}, min = Min, depth = Depth};
        _ ->
            throw({error, {badkey, K}})
    end.


get_left(N, Memory) when is_integer(N) ->
    get_left(maps:get(N, Memory), Memory);
get_left(#node{left = ?Undefined} = Node, _Memory) ->
    Node;
get_left(#node{left = Left}, Memory) ->
    get_left(maps:get(Left, Memory), Memory).