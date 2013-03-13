%% Feel free to use, reuse and abuse the code in this file.

%% @doc Stream handler for clock synchronizing.
-module(stream_handler).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-define(PERIOD, 100000).

init(_Transport, Req, _Opts, _Active) ->
	io:format("bullet init~n"),
	_ = erlang:send_after(?PERIOD, self(), refresh),
	{ok, Req, undefined}.

stream(<<"ping">>, Req, State) ->
	io:format("ping received~n"),
	{reply, <<"pong">>, Req, State};

stream(Data, Req, State) ->
    io:format("Data before conversation ~p~n", [Data]),
    Json = jiffy:decode(Data),
    io:format("astream received ~p~n", [Json]),
    {ok, Req, State}.

info(refresh, Req, State) ->
	_ = erlang:send_after(?PERIOD, self(), refresh),
	DateTime = cowboy_clock:rfc1123(),
	io:format("clock refresh timeout: ~s~n", [DateTime]),
	{reply, DateTime, Req, State};
info(Info, Req, State) ->
	io:format("info received ~p~n", [Info]),
	{ok, Req, State}.

terminate(_Req, _State) ->
	io:format("bullet terminate~n"),
	ok.
