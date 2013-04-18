-module(http_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(
		 [{c_utils:get_env(http, host, '_'), % host match
		   [
		         {"/deps/bullet/[...]", cowboy_static,
			  [{directory, {priv_dir, bullet, []}},
			   {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},

			 {"/bullet", bullet_handler, [{handler, stream_handler}]},
			 
			 {"/", cowboy_static,
			  [{directory, <<"./www">>},
			   {file, <<"index.html">>},
			   {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]},
			 
			 {"/[...]", cowboy_static,
			  [{directory, <<"./www">>},
			   {mimetypes, {fun mimetypes:path_to_mimes/2, default}}]}
			]}]),
    
    {ok, _} = cowboy:start_http(http, c_utils:get_env(http, pool_count, 100),
				[{port, c_utils:get_env(http, port, 8765)},
				 {max_connections,
				  c_utils:get_env(http,max_connections,infinity)}],
				[{env, [{dispatch, Dispatch}]}]).


stop(_State) ->
    ok.
