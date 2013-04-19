-module(http_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================
-spec start(normal | {takeover, node()} | {failover, node()},
            any()) -> {ok, pid()} | {ok, pid(), State::any()} |
                      {error, Reason::any()}.
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
				[{env, [{dispatch, Dispatch}]}]),

    %% Start main supervisor
    case http_sup:start_link() of
	{ok, Pid} ->
	    {ok, Pid};
	Error ->
	    Error
		end.


-spec stop(State::any()) -> ok.
stop(_State) ->
    ok.
