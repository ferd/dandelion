%%%-------------------------------------------------------------------
%% @doc dandelion tcp acceptors' supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dandelion_acceptors_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    {ok, Port} = application:get_env(dandelion, port),
    {ok, Acceptors} = application:get_env(dandelion, acceptors),
    {ok, Listen} = gen_tcp:listen(Port, [{active, false}, binary, {reuseaddr, true}]),
    SupFlags = #{strategy => one_for_one,
                 intensity => 10,
                 period => 1},
    ChildSpecs = [#{id => {acceptor, N},
                    start => {dandelion_acceptor, start_link, [Listen]}}
                  || N <- lists:seq(1,Acceptors)],
    {ok, {SupFlags, ChildSpecs}}.


