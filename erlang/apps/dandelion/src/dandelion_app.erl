%%%-------------------------------------------------------------------
%% @doc dandelion public API
%% @end
%%%-------------------------------------------------------------------

-module(dandelion_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    dandelion_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
