-module(fibheap).
-author("mmooyyii").

-export([new/0, insert/2, get_min/1, pop/1, heap_size/1]).

-define(Undefined, undefined).
-type fibHeap() :: tuple().

-record(fib_heap, {min, memory}).
-record(node, {left, right, child, key, val}).


-spec new() -> fibHeap().
new() ->
    #fib_heap{memory = maps:new()}.

-spec insert(fibHeap(), term()) -> fibHeap().
insert(#fib_heap{memory = Memory, min = ?Undefined}, {K, V}) ->
    Node = #node{key = K, val = V},
    #fib_heap{min = K, memory = Memory#{K => Node}};

insert(#fib_heap{memory = Memory, min = Min}, {K, V}) ->
    case maps:is_key(K, Memory) of
        true ->
            Node = maps:get(K, Memory),
            #fib_heap{memory = Memory#{K=>Node#node{val = V}}, min = Min};
        _ ->
            NewMin = case K < Min of true -> K; _ -> Min end,
            Root = #node{left = RootLeft} = maps:get(Min, Memory),
            Node = #node{left = RootLeft, right = Min, key = K, val = V},
            case RootLeft of
                ?Undefined ->
                    #fib_heap{
                        memory = Memory#{
                            K => Node,
                            Min => Root#node{left = K}
                        },
                        min = NewMin};
                _ ->
                    Left = maps:get(RootLeft, Memory),
                    #fib_heap{
                        memory = Memory#{
                            K => Node,
                            Min => Root#node{left = K},
                            RootLeft => Left#node{right = K}
                        },
                        min = NewMin}
            end
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
    {empty, #fib_heap{min = ?Undefined, memory = #{}}};
pop(#fib_heap{memory = Memory, min = K} = H) ->
    #node{left = Left, right = Right, child = Child} = maps:get(K, Memory),
    LeftN = maps:get(Left, Memory, ?Undefined),
    RightN = maps:get(Right, Memory, ?Undefined),
    #node{key = ReturnK, val = ReturnV} = maps:get(K, Memory),
    case Child of
        ?Undefined ->
            case heap_size(H) of
                1 ->
                    {{ReturnK, ReturnV}, #fib_heap{min = ?Undefined, memory = #{}}};
                _ ->
                    M = link_node(LeftN, RightN, Memory),
                    #node{key = Min} = get_left(get_root([Left, Right]), M),
                    NewMemory = maps:remove(K, M),
                    {{ReturnK, ReturnV}, consolidate(#fib_heap{memory = NewMemory, min = Min})}
            end;
        _ ->
            NewMemory = maps:remove(K, Memory),
            ChildLeft = get_left(Child, NewMemory),
            ChildRight = get_right(Child, NewMemory),
            case ChildLeft == ChildRight of
                true ->
                    M1 = link_node2(LeftN, RightN, ChildLeft, NewMemory),
                    #node{key = Min} = get_left(Child, M1),
                    {{ReturnK, ReturnV}, consolidate(#fib_heap{memory = M1, min = Min})};
                false ->
                    M1 = link_node(LeftN, ChildLeft, NewMemory),
                    M2 = link_node(ChildRight, RightN, M1),
                    #node{key = Min} = get_left(Child, M2),
                    {{ReturnK, ReturnV}, consolidate(#fib_heap{memory = M2, min = Min})}
            end
    end.

-spec heap_size(fibHeap()) -> integer().
heap_size(#fib_heap{memory = Memory}) ->
    maps:size(Memory).

consolidate(#fib_heap{min = K} = H) ->
    Size = ceil(math:log2(heap_size(H))),
    MutexTuple = erlang:list_to_tuple([0 || _ <- lists:seq(1, Size + 1)]),
    p_consolidate(H, K, MutexTuple).

p_consolidate(H, ?Undefined, _MutexTuple) ->
    H;
p_consolidate(#fib_heap{memory = Memory, min = Min} = H, K, MutexTuple) ->
    Degree = degree(K, Memory),
    Node = #node{right = NextK} = maps:get(K, Memory),
    case erlang:element(Degree, MutexTuple) of
        0 ->
            NewTuple = erlang:setelement(Degree, MutexTuple, Node),
            p_consolidate(H#fib_heap{min = min(Min, K)}, NextK, NewTuple);
        #node{key = Key1} ->
            #node{key = Key2} = maps:get(K, Memory),
            {NewHeap, NewMutex} = merge_node(Key1, Key2, H, Degree, MutexTuple),
            p_consolidate(NewHeap, NextK, NewMutex)
    end.

merge_node(Key1, Key2, #fib_heap{min = Min} = H, Degree, MutexTuple) ->
    case Key1 > Key2 of
        true ->
            p_merge_node(Key1, Key2, H#fib_heap{min = min(Key2, Min)}, Degree, MutexTuple);
        false ->
            p_merge_node(Key2, Key1, H#fib_heap{min = min(Key1, Min)}, Degree, MutexTuple)
    end.

p_merge_node(BigKey, SmallKey, #fib_heap{memory = Memory, min = Min}, Degree, MutexTuple) ->
    M1 = remove_node_from_top(BigKey, Memory),
    SmallNode = maps:get(SmallKey, M1),
    TmpTuple = erlang:setelement(Degree, MutexTuple, 0),
    BigNode = maps:get(BigKey, M1),
    NewHeap =
        case SmallNode of
            #node{child = ?Undefined} ->
                #fib_heap{memory = M1#{SmallKey => SmallNode#node{child = BigKey}}, min = Min};
            #node{child = Child} ->
                Left = #node{key = TmpKey} = get_left(Child, M1),
                #fib_heap{memory = M1#{TmpKey => Left#node{left = BigKey}, BigKey => BigNode#node{right = TmpKey}}, min = Min}
        end,
    case erlang:element(Degree + 1, TmpTuple) of
        0 ->
            NewTuple = erlang:setelement(Degree + 1, TmpTuple, SmallNode#node{child = BigKey}),
            {NewHeap, NewTuple};
        #node{key = OtherKey} ->
            merge_node(SmallKey, OtherKey, NewHeap, Degree + 1, TmpTuple)
    end.

remove_node_from_top(Index, Memory) when is_integer(Index) ->
    remove_node_from_top(maps:get(Index, Memory), Memory);

remove_node_from_top(#node{left = ?Undefined, right = ?Undefined}, Memory) ->
    Memory;
remove_node_from_top(#node{left = Left, right = ?Undefined, key = Key} = N, Memory) ->
    Node = maps:get(Left, Memory),
    Memory#{
        Left => Node#node{right = ?Undefined},
        Key => N#node{left = ?Undefined, right = ?Undefined}
    };
remove_node_from_top(#node{left = ?Undefined, right = Right, key = Key} = N, Memory) ->
    Node = maps:get(Right, Memory),
    Memory#{
        Right => Node#node{left = ?Undefined},
        Key => N#node{left = ?Undefined, right = ?Undefined}
    };
remove_node_from_top(#node{left = Left, right = Right, key = Key} = N, Memory) ->
    Node1 = #node{key = Key1} = maps:get(Left, Memory),
    Node2 = #node{key = Key2} = maps:get(Right, Memory),
    Memory#{
        Left => Node1#node{right = Key2},
        Right => Node2#node{left = Key1},
        Key => N#node{left = ?Undefined, right = ?Undefined}
    }.

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

link_node(?Undefined, Node2 = #node{key = Key2}, Memory) ->
    Memory#{Key2 => Node2#node{left = ?Undefined}};
link_node(Node1 = #node{key = Key1}, ?Undefined, Memory) ->
    Memory#{Key1 => Node1#node{right = ?Undefined}};
link_node(Node1 = #node{key = Key1}, Node2 = #node{key = Key2}, Memory) ->
    N1 = Node1#node{right = Key2},
    N2 = Node2#node{left = Key1},
    Memory#{Key1=>N1, Key2=>N2}.

%% 当 ChildLeft == ChildRight 时，node左右都为undefined
link_node2(?Undefined, ?Undefined, _Node, Memory) ->
    Memory;
link_node2(Left = #node{key = LeftKey}, ?Undefined, Node = #node{key = Key}, Memory) ->
    Memory#{LeftKey => Left#node{right = Key}, Key => Node#node{left = LeftKey}};
link_node2(?Undefined, Right = #node{key = RightKey}, Node = #node{key = Key}, Memory) ->
    Memory#{RightKey => Right#node{left = Key}, Key => Node#node{right = RightKey}};
link_node2(Left = #node{key = LeftKey}, Right = #node{key = RightKey}, Node = #node{key = Key}, Memory) ->
    Memory#{
        LeftKey => Left#node{right = Key},
        RightKey => Right#node{left = Key},
        Key => Node#node{left = LeftKey, right = RightKey}
    }.

%% get one of node in top row
get_root([]) ->
    ?Undefined;
get_root([?Undefined | Rest]) ->
    get_root(Rest);
get_root([NodeIndex | _]) ->
    NodeIndex.


degree(N, Memo) ->
    degree(N, Memo, 0).
degree(?Undefined, _Memo, Acc) ->
    Acc;
degree(N, Memo, Acc) ->
    #node{child = Child} = maps:get(N, Memo),
    degree(Child, Memo, Acc + 1).