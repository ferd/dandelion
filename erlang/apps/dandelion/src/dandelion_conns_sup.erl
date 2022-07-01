%%%-------------------------------------------------------------------
%% @doc dandelion tcp connections supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(dandelion_conns_sup).

-behaviour(supervisor).

-export([start_link/0, start_conn/1]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_conn(Sock) ->
    supervisor:start_child(?SERVER, [Sock]).

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
    SupFlags = #{strategy => simple_one_for_one,
                 intensity => 100,
                 period => 1},
    ChildSpecs = [
        #{id => conn,
          start => {dandelion_conn, start_link, []}}
    ],
    {ok, {SupFlags, ChildSpecs}}.



