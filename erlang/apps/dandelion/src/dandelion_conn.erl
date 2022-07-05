%%%-------------------------------------------------------------------
%% @doc dandelion connection handler. This is where
%% the business logic lives for now.
%% @end
%%%-------------------------------------------------------------------
-module(dandelion_conn).
-behaviour(gen_server).

-define(CONTINUE_WAIT, 500).
-define(DISPLAY_DELAY, timer:seconds(2)).

-export([start_link/1]).
-export([init/1, handle_continue/2, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Sock) ->
    gen_server:start_link(?MODULE, [Sock], []).

init([Sock]) ->
    {ok, #{sock => Sock, buffer => <<>>},
     {continue, wait_control}}.

handle_continue(wait_control, State=#{sock := Sock}) ->
    %% The wait shouldn't be long
    receive
        {ready, Sock} ->
            display_event(),
            inet:setopts(Sock, [{active, once}]),
            {noreply, State}
    after ?CONTINUE_WAIT ->
        {noreply, State, {continue, wait_control}}
    end.

handle_call(_Call, _From, State) ->
    {noreply, State}.

handle_cast(_Cast, State) ->
    {noreply, State}.

handle_info(display, State=#{sock := Sock}) ->
    Str = "    \n"
          "   @\n"
          " \\ |\n"
          "__\\!/__\n\n",
    gen_tcp:send(Sock, Str),
    display_event(),
    {noreply, State};
handle_info({tcp, Sock, _Msg}, State=#{sock := Sock}) ->
    inet:setopts(Sock, [{active, once}]),
    {ok, Vsn} = application:get_key(dandelion, vsn),
    gen_tcp:send(Sock, "vsn: " ++ Vsn ++ "\n"),
    {noreply, State};
handle_info({tcp_error, _Sock, Reason}, State) ->
    {stop, {shutdown, {sock, Reason}}, State};
handle_info({tcp_closed, _Sock}, State) ->
    {stop, {shutdown, {sock, closed}}, State}.

display_event() ->
    erlang:send_after(?DISPLAY_DELAY, self(), display).
