%% @doc Stream handler for bullet.
-module(stream_handler).

-export([init/4]).
-export([stream/3]).
-export([info/3]).
-export([terminate/2]).

init(_Transport, Req, _Opts, _Active) ->
    error_logger:info_report("Initializing bullet handler"),
    {ok, Req, h_chat_adapter:initial_state()}.

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
    h_chat_adapter:terminate(State).

