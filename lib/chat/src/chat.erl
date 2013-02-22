%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created :  3 Feb 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(chat).


%% API
-export([start/0, stop/0, create_room/0, create_room/1, stop_room/1,
	 create_user/0, create_user/2, stop_user/1, get_room_count/0,
	 get_user_nick/1, get_user_count/0]).

%%%===================================================================
%%% API
%%%===================================================================
%%--------------------------------------------------------------------
%% @doc
%% Starts chat server
%% @spec start() -> ok
%% @end
%%--------------------------------------------------------------------
start() ->
    application:start(chat).

%%--------------------------------------------------------------------
%% @doc
%% Starts chat server
%% @spec stop() -> ok
%% @end
%%--------------------------------------------------------------------
stop() ->
    application:stop(chat).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new room
%% @spec create_room() -> {ok, Room_code}
%% @end
%%--------------------------------------------------------------------
create_room()->
    create_room(c_utils:generate_code()).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new room with given Room_code
%% @spec create_room(Room_code) -> {ok, Room_code}
%% @end
%%--------------------------------------------------------------------
create_room(Code) ->
    case c_room_sup:start_child(Code) of
	{ok, Pid} ->
	    c_room:get_code(Pid);
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new room
%% @spec create_room() -> {ok, Room_code}
%% @end
%%--------------------------------------------------------------------
create_user()->
    Code = c_utils:generate_code(),
    create_user(Code, Code).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new room with given Room_code
%% @spec create_room(Room_code) -> {ok, Room_code}
%% @end
%%--------------------------------------------------------------------
create_user(Code, Nick) ->
    case c_user_sup:start_child(Code, Nick) of
	{ok, Pid} ->
	    c_user:get_code(Pid);
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Stops room with given code
%% @spec stop_room(Room_code) -> ok
%% @end
%%--------------------------------------------------------------------
stop_room(Code) ->
    case c_room:get_room(Code) of
	{ok, Pid} ->
	    c_room:stop(Pid);
	Error ->
	    Error
		end.

%%--------------------------------------------------------------------
%% @doc
%% Stops room with given code
%% @spec stop_room(Room_code) -> ok
%% @end
%%--------------------------------------------------------------------
stop_user(Code) ->
    case c_user:get_user(Code) of
	{ok, Pid} ->
	    c_user:stop(Pid);
	Error ->
	    Error
		end.

%%--------------------------------------------------------------------
%% @doc
%% Gives number of room counts in the system
%% @spec get_room_count() -> Room_count
%% @end
%%--------------------------------------------------------------------
get_room_count()->
    c_room:get_count().

%%--------------------------------------------------------------------
%% @doc
%% Gives number of room counts in the system
%% @spec get_room_count() -> Room_count
%% @end
%%--------------------------------------------------------------------
get_user_count()->
    c_user:get_count().

%%--------------------------------------------------------------------
%% @doc
%% Gets pid of given user_code for given room_code
%% @spec get_user_nick(Room_code, User_code) -> {ok, User_nick}
%% @end
%%--------------------------------------------------------------------
get_user_nick(Code) ->
    case c_user:get_user(Code) of
	{ok, Pid} ->
	    c_user:get_nick(Pid);
	Error ->
	    Error
    end.
