-module(fibheap).
-author("mmooyyii").

-export([new/0, insert/2, get_min/1, pop/1, heap_size/1]).

-define(Undefined, undefined).
-type fibHeap() :: tuple().

-record(fib_heap, {min, memory}).
-record(node, {left, right, child, key, val, ref}).


-spec new() -> fibHeap().
new() ->
    #fib_heap{memory = maps:new()}.

-spec insert(fibHeap(), term()) -> fibHeap().
insert(#fib_heap{memory = Memory, min = ?Undefined}, {K, V}) ->
    Node = #node{ref = Ref} = new_node(K, V),
    #fib_heap{min = Ref, memory = Memory#{Ref => Node}};

insert(#fib_heap{memory = Memory, min = MinRef}, {K, V}) ->
    #node{key = Min} = get_node(MinRef, Memory),
    Root = #node{right = RootRightRef} = get_node(MinRef, Memory),
    TmpNode = #node{ref = NodeRef} = new_node(K, V),
    Node = TmpNode#node{left = MinRef, right = RootRightRef},
    NewMinRef = compare_min({K, NodeRef}, {Min, MinRef}),
    case RootRightRef of
        ?Undefined ->
            #fib_heap{
                memory = Memory#{
                    NodeRef => Node,
                    MinRef => Root#node{right = NodeRef}
                },
                min = NewMinRef};
        _ ->
            Right = get_node(RootRightRef, Memory),
            #fib_heap{
                memory = Memory#{
                    NodeRef => Node,
                    MinRef => Root#node{right = NodeRef},
                    RootRightRef => Right#node{left = NodeRef}
                },
                min = NewMinRef}
    end.

-spec get_min(fibHeap()) -> term().
get_min(F = #fib_heap{min = ?Undefined}) ->
    {empty, F};
get_min(F = #fib_heap{memory = Memory, min = MinRef}) ->
    #node{val = Val, key = Key} = get_node(MinRef, Memory),
    {{Key, Val}, F}.

-spec pop(fibHeap()) -> term().
pop(F = #fib_heap{min = ?Undefined}) ->
    {empty, F};
pop(#fib_heap{memory = Memory, min = MinRef} = F) ->
    #node{left = Left, right = Right, child = Child} = get_node(MinRef, Memory),
    LeftNode = get_node(Left, Memory),
    RightNode = get_node(Right, Memory),
    #node{key = ReturnK, val = ReturnV} = get_node(MinRef, Memory),
    case Child of
        ?Undefined ->
            case heap_size(F) of
                1 ->
                    {{ReturnK, ReturnV}, #fib_heap{min = ?Undefined, memory = #{}}};
                _ ->
                    M = link_node(LeftNode, RightNode, Memory),
                    #node{ref = RootRef} = get_most_left(get_any_node([Left, Right]), M),
                    NewMemory = delete_node(MinRef, M),
                    {{ReturnK, ReturnV}, consolidate(#fib_heap{memory = NewMemory, min = RootRef})}
            end;
        _ ->
            NewMemory = delete_node(MinRef, Memory),
            ChildLeft = get_most_left(Child, NewMemory),
            ChildRight = get_most_right(Child, NewMemory),
            case ChildLeft == ChildRight of
                true ->
                    M1 = link_node2(LeftNode, RightNode, ChildLeft, NewMemory),
                    #node{ref = RootRef} = get_most_left(Child, M1),
                    {{ReturnK, ReturnV}, consolidate(#fib_heap{memory = M1, min = RootRef})};
                false ->
                    M1 = link_node(LeftNode, ChildLeft, NewMemory),
                    M2 = link_node(ChildRight, RightNode, M1),
                    #node{ref = RootRef} = get_most_left(Child, M2),
                    {{ReturnK, ReturnV}, consolidate(#fib_heap{memory = M2, min = RootRef})}
            end
    end.

-spec heap_size(fibHeap()) -> integer().
heap_size(#fib_heap{memory = Memory}) ->
    maps:size(Memory).

consolidate(#fib_heap{min = StartRef} = H) ->
    Size = ceil(math:log2(heap_size(H))),
    MutexTuple = erlang:list_to_tuple([0 || _ <- lists:seq(1, Size + 1)]),
    p_consolidate(H, StartRef, MutexTuple).

p_consolidate(H, ?Undefined, _MutexTuple) ->
    H;
p_consolidate(#fib_heap{memory = Memory, min = MinRef} = F, NowRef = Ref2, MutexTuple) ->
    Degree = degree(NowRef, Memory),
    Node = #node{right = NextRef, key = NowKey = Key2} = get_node(NowRef, Memory),
    #node{key = MinKey} = get_node(MinRef, Memory),
    case erlang:element(Degree, MutexTuple) of
        0 ->
            NewMutex = erlang:setelement(Degree, MutexTuple, Node),
            p_consolidate(F#fib_heap{min = compare_min({MinKey, MinRef}, {NowKey, NowRef})}, NextRef, NewMutex);
        #node{key = Key1, ref = Ref1} ->
            {NewHeap, NewMutex} = merge_node({Key1, Ref1}, {Key2, Ref2}, {MinKey, MinRef}, F, Degree, MutexTuple),
            p_consolidate(NewHeap, NextRef, NewMutex)
    end.

merge_node({Key1, Ref1}, {Key2, Ref2}, H, Degree, MutexTuple) when Key1 > Key2 ->
    p_merge_node(Ref1, Ref2, H, Degree, MutexTuple);
merge_node({_Key1, Ref1}, {_Key2, Ref2}, H, Degree, MutexTuple) ->
    p_merge_node(Ref2, Ref1, H, Degree, MutexTuple).

merge_node({Key1, Ref1}, {Key2, Ref2}, {MinKey, MinRef}, H, Degree, MutexTuple) when Key1 > Key2 ->
    p_merge_node(Ref1, Ref2, H#fib_heap{min = compare_min({Key2, Ref2}, {MinKey, MinRef})}, Degree, MutexTuple);
merge_node({Key1, Ref1}, {_Key2, Ref2}, {MinKey, MinRef}, H, Degree, MutexTuple) ->
    p_merge_node(Ref2, Ref1, H#fib_heap{min = compare_min({Key1, Ref1}, {MinKey, MinRef})}, Degree, MutexTuple).

p_merge_node(BigRef, SmallRef, #fib_heap{memory = Memory, min = Min}, Degree, MutexTuple) ->
    M1 = remove_node_from_top(BigRef, Memory),
    SmallNode = #node{key = SmallKey} = get_node(SmallRef, M1),
    TmpTuple = erlang:setelement(Degree, MutexTuple, 0),
    BigNode = get_node(BigRef, M1),
    NewHeap =
        case SmallNode of
            #node{child = ?Undefined} ->
                #fib_heap{memory = M1#{SmallRef => SmallNode#node{child = BigRef}}, min = Min};
            #node{child = Child} ->
                Left = #node{ref = MostLeftRef} = get_most_left(Child, M1),
                #fib_heap{memory = M1#{
                    MostLeftRef => Left#node{left = BigRef},
                    BigRef => BigNode#node{right = MostLeftRef},
                    SmallRef => SmallNode#node{child = BigRef}
                }, min = Min}
        end,
    case erlang:element(Degree + 1, TmpTuple) of
        0 ->
            NewTuple = erlang:setelement(Degree + 1, TmpTuple, SmallNode#node{child = BigRef}),
            {NewHeap, NewTuple};
        #node{ref = OtherRef} ->
            #fib_heap{memory = TmpMemory} = NewHeap,
            #node{key = OtherKey} = get_node(OtherRef, TmpMemory),
            merge_node({SmallKey, SmallRef}, {OtherKey, OtherRef}, NewHeap, Degree + 1, TmpTuple)
    end.

remove_node_from_top(Ref, Memory) when is_reference(Ref) ->
    remove_node_from_top(get_node(Ref, Memory), Memory);
remove_node_from_top(#node{left = ?Undefined, right = ?Undefined}, Memory) ->
    Memory;
remove_node_from_top(#node{left = Left, right = ?Undefined, ref = Ref} = N, Memory) ->
    Node = get_node(Left, Memory),
    Memory#{
        Left => Node#node{right = ?Undefined},
        Ref => N#node{left = ?Undefined, right = ?Undefined}
    };
remove_node_from_top(#node{left = ?Undefined, right = Right, ref = Ref} = N, Memory) ->
    Node = get_node(Right, Memory),
    Memory#{
        Right => Node#node{left = ?Undefined},
        Ref => N#node{left = ?Undefined, right = ?Undefined}
    };
remove_node_from_top(#node{left = Left, right = Right, ref = Ref} = N, Memory) ->
    Node1 = #node{ref = Ref1} = get_node(Left, Memory),
    Node2 = #node{ref = Ref2} = get_node(Right, Memory),
    Memory#{
        Left => Node1#node{right = Ref2},
        Right => Node2#node{left = Ref1},
        Ref => N#node{left = ?Undefined, right = ?Undefined}
    }.

get_most_left(Ref, Memory) when is_reference(Ref) ->
    get_most_left(get_node(Ref, Memory), Memory);
get_most_left(#node{left = ?Undefined} = Node, _Memory) ->
    Node;
get_most_left(#node{left = Left}, Memory) ->
    get_most_left(get_node(Left, Memory), Memory).

get_most_right(Ref, Memory) when is_reference(Ref) ->
    get_most_right(get_node(Ref, Memory), Memory);
get_most_right(#node{right = ?Undefined} = Node, _Memory) ->
    Node;
get_most_right(#node{right = Right}, Memory) ->
    get_most_right(get_node(Right, Memory), Memory).

link_node(?Undefined, Node2 = #node{ref = Ref2}, Memory) ->
    Memory#{Ref2 => Node2#node{left = ?Undefined}};
link_node(Node1 = #node{ref = Ref1}, ?Undefined, Memory) ->
    Memory#{Ref1 => Node1#node{right = ?Undefined}};
link_node(Node1 = #node{ref = Ref1}, Node2 = #node{ref = Ref2}, Memory) ->
    N1 = Node1#node{right = Ref2},
    N2 = Node2#node{left = Ref1},
    Memory#{Ref1=>N1, Ref2=>N2}.

%% 当 ChildLeft == ChildRight 时，node左右都为undefined
link_node2(?Undefined, ?Undefined, _Node, Memory) ->
    Memory;
link_node2(Left = #node{ref = LeftRef}, ?Undefined, Node = #node{ref = Ref}, Memory) ->
    Memory#{LeftRef => Left#node{right = Ref}, Ref => Node#node{left = LeftRef}};
link_node2(?Undefined, Right = #node{ref = RightRef}, Node = #node{ref = Ref}, Memory) ->
    Memory#{RightRef => Right#node{left = Ref}, Ref => Node#node{right = RightRef}};
link_node2(Left = #node{ref = LeftRef}, Right = #node{ref = RightRef}, Node = #node{ref = Ref}, Memory) ->
    Memory#{
        LeftRef => Left#node{right = Ref},
        RightRef => Right#node{left = Ref},
        Ref => Node#node{left = LeftRef, right = RightRef}
    }.

%% get one of node in top row
get_any_node([]) ->
    ?Undefined;
get_any_node([?Undefined | Rest]) ->
    get_any_node(Rest);
get_any_node([NodeIndex | _]) ->
    NodeIndex.


degree(N, Memo) ->
    degree(N, Memo, 0).
degree(?Undefined, _Memo, Acc) ->
    Acc;
degree(N, Memory, Acc) ->
    #node{child = Child} = get_node(N, Memory),
    degree(Child, Memory, Acc + 1).


new_node(K, V) ->
    Ref = erlang:make_ref(),
    #node{key = K, val = V, ref = Ref}.

get_node(?Undefined, _) ->
    ?Undefined;
get_node(Ref, Memory) ->
    maps:get(Ref, Memory).

compare_min({Key1, Node1Ref}, {Key2, _Node2Ref}) when Key1 < Key2 ->
    Node1Ref;
compare_min(_, {_Node2, Node2Ref}) ->
    Node2Ref.

delete_node(Ref, Memory) ->
    maps:remove(Ref, Memory).