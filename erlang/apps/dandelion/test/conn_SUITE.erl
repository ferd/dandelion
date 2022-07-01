-module(conn_SUITE).
-include_lib("stdlib/include/assert.hrl").
-include_lib("common_test/include/ct.hrl").
-compile(export_all).

all() ->
    [conn, nonblock].

init_per_testcase(_Case, Config) ->
    application:load(dandelion),
    Port = 9999,
    application:set_env(dandelion, port, Port),
    application:set_env(dandelion, acceptors, 1),
    {ok, Apps} = application:ensure_all_started(dandelion),
    [{apps, Apps}, {port, Port} | Config].

end_per_testcase(_Case, Config) ->
    [application:stop(App) || App <- lists:reverse(?config(apps, Config))],
    Config.

conn() ->
    [{doc, "A connection is established and responds to packets"},
     {timetrap, timer:seconds(5)}].
conn(Config) ->
    Port = ?config(port, Config),
    {ok, Sock} = gen_tcp:connect({127,0,0,1}, Port, [{active, false}]),
    ok = gen_tcp:send(Sock, "hello?"),
    {ok, R1} = gen_tcp:recv(Sock, 0),
    ?assertNotEqual(nomatch, string:find(R1, ".")),
    ok = gen_tcp:send(Sock, "hi!"),
    {ok, R2} = gen_tcp:recv(Sock, 0),
    ?assertNotEqual(nomatch, string:find(R2, ".")),
    ok = gen_tcp:send(Sock, "done."),
    {ok, R3} = gen_tcp:recv(Sock, 0),
    ?assertNotEqual(nomatch, string:find(R3, ".")),
    ok = gen_tcp:close(Sock),
    ok.

nonblock() ->
    [{doc, "The server survives disconnections and the acceptor "
           "is non-blocking"},
     {timetrap, timer:seconds(5)}].
nonblock(Config) ->
    Port = ?config(port, Config),
    {ok, Sock1} = gen_tcp:connect({127,0,0,1}, Port, [{active, false}]),
    {ok, Sock2} = gen_tcp:connect({127,0,0,1}, Port, [{active, false}]),
    ok = gen_tcp:send(Sock1, "hello?"),
    ok = gen_tcp:send(Sock2, "hello?"),
    {ok, _} = gen_tcp:recv(Sock1, 0),
    {ok, _} = gen_tcp:recv(Sock2, 0),
    ok = gen_tcp:close(Sock1),
    ok = gen_tcp:send(Sock2, "hello?"),
    {ok, _} = gen_tcp:recv(Sock2, 0),
    ok = gen_tcp:close(Sock2),
    ok.
