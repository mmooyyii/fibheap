-module(fibheap).
-author("mmooyyii").

-export([new/0, insert/2, get_min/1, pop/1, delete/2, heap_size/1]).

-define(Undefined, undefined).
-type fibHeap() :: tuple().
-record(fib_heap, {min, memory}).
-record(node, {left, right, child, parent, key, val}).


-spec new() -> fibHeap().
new() ->
    #fib_heap{memory = maps:new()}.

-spec insert(fibHeap(), term()) -> fibHeap().
insert(#fib_heap{memory = Memory, min = ?Undefined}, {K, V}) ->
    Node = #node{key = K, val = V},
    #fib_heap{min = K, memory = Memory#{K => Node}};

insert(F = #fib_heap{memory = Memory, min = Min}, {K, V}) ->
    case maps:is_key(K, Memory) of
        true ->
            Node = maps:get(Memory, K),
            #fib_heap{memory = Memory#{K=>Node#node{val = V}}, min = Min};
        _ ->
            NewMin = case K < Min of true -> Min; _ -> K end,
            Root = #node{left = RootLeft} = maps:get(Memory, F),
            Node = #node{left = RootLeft, right = Min, val = V},
            #fib_heap{memory = Memory#{K => Node, Min => Root#node{left = K}}, min = NewMin}
    end.

-spec get_min(fibHeap()) -> term().
get_min(F = #fib_heap{memory = Memory, min = K}) ->
    case K of
        ?Undefined -> {empty, F};
        _ -> #node{val = Val} = maps:get(K, Memory), {{K, Val}, F}
    end.

-spec pop(fibHeap()) -> term().
pop(#fib_heap{min = ?Undefined}) ->
%%  empty heap
    {empty, #fib_heap{min = -1, memory = #{}}};
pop(#fib_heap{memory = Memory, min = K} = H) ->
    #node{left = Left, right = Right, child = Child} = maps:get(Memory, K),
    LeftN = maps:get(Memory, Left, ?Undefined),
    RightN = maps:get(Memory, Right, ?Undefined),
    #node{key = ReturnK, val = ReturnV} = maps:get(Memory, K),
    case Child of
        ?Undefined ->
            case heap_size(H) of
                1 ->
                    {{ReturnK, ReturnV}, #fib_heap{min = -1, memory = #{}}};
                _ ->
                    Min = get_root([Left, Right]),
                    M = link_node(LeftN, RightN, Memory),
                    {{ReturnK, ReturnV}, consolidate(#fib_heap{memory = M, min = Min})}
            end;
        _ ->
            ChildLeft = get_left(Child, Memory),
            ChildRight = get_right(Child, Memory),
            M1 = link_node(LeftN, ChildLeft, Memory),
            M2 = link_node(ChildRight, RightN, M1),
            #node{key = Min} = get_left(get_root([Left, Right]), Memory),
            {{ReturnK, ReturnV}, consolidate(#fib_heap{memory = M2, min = Min})}
    end.

-spec delete(fibHeap(), term()) -> term().
delete(_, _) ->
    ok.

-spec heap_size(fibHeap()) -> integer().
heap_size(#fib_heap{memory = Memory}) ->
    maps:size(Memory).

consolidate(#fib_heap{min = K} = H) ->
    Size = ceil(math:log2(heap_size(H))),
    MutexTuple = erlang:list_to_tuple([0 || _ <- lists:seq(1, Size + 1)]),
    p_consolidate(H, K, MutexTuple).

p_consolidate(H, ?Undefined, _MutexTuple) ->
    H;
p_consolidate(#fib_heap{memory = Memory} = H, K, MutexTuple) ->
    Degree = degree(K, Memory),
    #node{right = NextK} = maps:get(Memory, K),
    case erlang:element(Degree, MutexTuple) of
        0 ->
            NewTuple = erlang:setelement(Degree, MutexTuple, K),
            #node{right = Right} = maps:get(Memory, K),
            p_consolidate(H, Right, NewTuple);
        #node{key = Key1} = Node1 ->
            #node{key = Key2} = Node2 = maps:get(Memory, K),
            case Key1 > Key2 of
                true ->
                    merge_node(Node1, Node2, Memory, Degree, MutexTuple, H, NextK);
                false ->
                    merge_node(Node2, Node1, Memory, Degree, MutexTuple, H, NextK)
            end
    end.

merge_node(Node1 = #node{key = Key1}, Node2 = #node{key = Key2}, Memory, Degree, MutexTuple, H, NextK) ->
    M1 = remove_node_from_top(Node1, Memory),
    case Node2 of
        #node{child = ?Undefined} ->
            M1#{Key2 => Node2#node{child = Key1}};
        #node{child = Child} ->
            Left = #{key = TmpKey} = get_left(Child, M1),
            M2 = M1#{TmpKey => Left#node{left = Key1}},
            TmpTuple = erlang:setelement(Degree, MutexTuple, 0),
            NewTuple = erlang:setelement(degree(Key2, M2), TmpTuple, Node2),
            p_consolidate(H#fib_heap{memory = M2}, NextK, NewTuple)
    end.

remove_node_from_top(#node{left = ?Undefined, right = ?Undefined}, Memory) ->
    Memory;
remove_node_from_top(#node{left = Left, right = ?Undefined}, Memory) ->
    Node = maps:get(Memory, Left),
    Memory#{Left => Node#node{right = ?Undefined}};
remove_node_from_top(#node{left = ?Undefined, right = Right}, Memory) ->
    Node = maps:get(Memory, Right),
    Memory#{Right => Node#node{left = ?Undefined}};
remove_node_from_top(#node{left = Left, right = Right}, Memory) ->
    Node1 = #node{key = Key1} = maps:get(Memory, Left),
    Node2 = #node{key = Key2} = maps:get(Memory, Right),
    Memory#{Left => Node1#node{right = Key2}, Right => Node2#node{left = Key1}}.


get_left(N, Memory) when is_integer(N) ->
    get_left(maps:get(N, Memory), Memory);
get_left(#node{left = ?Undefined} = Node, _Memory) ->
    Node;
get_left(#node{left = Left}, Memory) ->
    get_left(maps:get(Left, Memory), Memory).

get_right(N, Memory) when is_integer(N) ->
    get_right(maps:get(N, Memory), Memory);
get_right(#node{right = ?Undefined} = Node, _Memory) ->
    Node;
get_right(#node{right = Right}, Memory) ->
    get_right(maps:get(Right, Memory), Memory).

link_node(?Undefined, _, Memory) ->
    Memory;
link_node(_, ?Undefined, Memory) ->
    Memory;
link_node(Node1 = #node{key = Key1}, Node2 = #node{key = Key2}, Memory) ->
    N1 = Node1#node{right = Key2},
    N2 = Node2#node{left = Key1},
    Memory#{Key1=>N1, Key2=>N2}.


%% get one of node in top row
get_root([]) ->
    -1;
get_root([?Undefined | Rest]) ->
    get_root(Rest);
get_root([NodeIndex | _]) ->
    NodeIndex.


degree(N, Memo) ->
    degree(N, Memo, 0).
degree(?Undefined, _Memo, Acc) ->
    Acc;
degree(N, Memo, Acc) ->
    #node{child = Child} = maps:get(Memo, N),
    degree(Child, Memo, Acc + 1).
