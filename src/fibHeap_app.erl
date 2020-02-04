%%%-------------------------------------------------------------------
%% @doc fibHeap public API
%% @end
%%%-------------------------------------------------------------------

-module(fibHeap_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, self()}.

stop(_State) ->
    ok.

%% internal functions
