%% Feel free to use, reuse and abuse the code in this file.

%% @doc Stream handler for clock synchronizing.
-module(stream_handler).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

-define(PERIOD, 100000).

init(_Transport, Req, _Opts, _Active) ->
    error_logger:info_report("Initializing bullet handler"),
    _ = erlang:send_after(?PERIOD, self(), refresh),
    {ok, Req, undefined}.

stream(<<"ping">>, Req, State) ->
    error_logger:info_report(ping_received),
    {reply, <<"pong">>, Req, State};

stream(Raw_data, Req, State) ->
    error_logger:info_report({raw_request, Raw_data}),
    Data = jiffy:decode(Raw_data),
    error_logger:info_report({processed_request, Data}),
    h_chat_adapter:handle_request(Data, Req, State).

info(refresh, Req, State) ->
    _ = erlang:send_after(?PERIOD, self(), refresh),
    DateTime = cowboy_clock:rfc1123(),
    error_logger:info_report({clock_refresh_timeout, DateTime}),
    {reply, DateTime, Req, State};

info(Info, Req, State) ->
    error_logger:info_report({info_received, Info}),
    {ok, Req, State}.

terminate(_Req, _State) ->
    error_logger:info_report(terminate_bullet_handler),
    ok.
