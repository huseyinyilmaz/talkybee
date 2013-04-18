%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 19 Feb 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(c_utils).

%% API
-export([generate_code/0, get_env/3, set_env/3]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Generates random code as binary
%% @end
%%--------------------------------------------------------------------
generate_code()->
    list_to_binary(
      integer_to_list(erlang:phash2({node(), now()}), 36)).

%%--------------------------------------------------------------------
%% @doc
%% gets configuration for given app from sys.config file.
%% if configuration does not exists, it uses given default value.
%% @end
%%--------------------------------------------------------------------
-spec get_env(atom(), atom(), any()) -> any().
get_env(AppName, Key, Default) ->
    case application:get_env(AppName, Key) of
	undefined -> Default;
	{ok, Value} -> Value
    end.

%%--------------------------------------------------------------------
%% @doc
%% Set environment variable for given app
%% @end
%%--------------------------------------------------------------------
-spec set_env(atom(), atom(), any()) -> any().
set_env(AppName, Key, Value) ->
    application:set_env(AppName, Key, Value).
%%%===================================================================
%%% Internal functions
%%%===================================================================

