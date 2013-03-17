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
    {ok, Req, undefined}.

stream(Raw_data, Req, State) ->
    error_logger:info_report({raw_request, Raw_data}),
    Data = jiffy:decode(Raw_data),
    error_logger:info_report({processed_request, Data}),
    h_chat_adapter:handle_request(Data, Req, State).

info(Data, Req, State) ->
    error_logger:info_report({info, Data}),
    h_chat_adapter:handle_info(Data, Req, State).

terminate(_Req, State) ->
    error_logger:info_report(terminate_bullet_handler),
    case State of
	undefined -> ok;
	User_code -> chat:stop_user(User_code)
    end.

