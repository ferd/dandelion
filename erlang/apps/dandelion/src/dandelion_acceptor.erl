%%%-------------------------------------------------------------------
%% @doc dandelion tcp acceptor. Looks for a connection, and then
%% hands it off to a connection holder in a little dance.
%% @end
%%%-------------------------------------------------------------------
-module(dandelion_acceptor).
-behaviour(gen_server).
-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(ACCEPT_WAIT, 250).

start_link(LSock) ->
    gen_server:start_link(?MODULE, [LSock], []).

init([LSock]) ->
    self() ! accept,
    {ok, #{listen => LSock}}.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(accept, State = #{listen := LSock}) ->
    %% We use an interruptible accept mechanism so that we can do be
    %% stalled by a live code upgrade that changes state before moving on.
    self() ! accept,
    case gen_tcp:accept(LSock, ?ACCEPT_WAIT) of
        {error, _Reason} ->
            {noreply, State};
        {ok, Sock} ->
            {ok, Pid} = dandelion_conns_sup:start_conn(Sock),
            ok = gen_tcp:controlling_process(Sock, Pid),
            Pid ! {ready, Sock},
            {noreply, State}
    end.
