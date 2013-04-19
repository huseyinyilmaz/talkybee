%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 18 Feb 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(http).

%% API
-export([start/0, stop/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts http server
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
	application:start(crypto),
	application:start(ranch),
	application:start(cowboy),
        application:start(http).

%%--------------------------------------------------------------------
%% @doc
%% Starts http server
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    application:stop(http),
    application:stop(cowboy),
    application:stop(ranch),
    application:stop(crypto).


%%%===================================================================
%%% Internal functions
%%%===================================================================
