-module(http_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(
		 [{'_', [%% {"/bullet/", h_room_handler, []}
			 
			 %% },
			 {"/[...]", cowboy_static,
			  [{directory, <<"./www">>},
			   {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}
			]}]),
    
    {ok, _} = cowboy:start_http(http, 100, [{port, 8080}],
				[{env, [{dispatch, Dispatch}]}]).


stop(_State) ->
    ok.
