# fibheap
Fibonacci heap for erlang

EXAMPLE:
    
    F1 = fibheap:new(),
    F2 = fibheap:insert(F1, {1, 1}),
    F3 = fibheap:insert(F2, {2, 2}),
    {{1, 1}, F6} = fibheap:pop(F3),
    {{2, 2}, F7} = fibheap:pop(F6).
