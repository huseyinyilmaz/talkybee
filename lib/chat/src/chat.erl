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
	 get_room_count/0, create_user/1, create_user/2, create_user/3,
	 get_user_nick/2, get_user_count/1, delete_user/2,
	 send_message/3]).


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
    case c_room_sup:start_child() of
	{ok, Room} ->
	    c_room:get_code(Room);
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new room with given Room_code
%% @spec create_room(Room_code) -> {ok, Room_code}
%% @end
%%--------------------------------------------------------------------
create_room(Room_code) ->
    case c_room_sup:start_child(Room_code) of
	{ok, Room} ->
	    c_room:get_code(Room);
	Error -> Error
    end.


%%--------------------------------------------------------------------
%% @doc
%% Stops room with given code
%% @spec stop_room(Room_code) -> ok
%% @end
%%--------------------------------------------------------------------
stop_room(Room_code) ->
    case c_room:get_room(Room_code) of
	{ok, Pid} ->
	    c_room:stop(Pid);
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
    c_room:get_room_count().


%%--------------------------------------------------------------------
%% @doc
%% Creates a new user for given Room_code
%% @spec create_user(Room_code) -> {ok, Room_code}
%% @end
%%--------------------------------------------------------------------
create_user(Room_code) ->
    {ok, Pid} = c_room:get_room(Room_code),
    c_room:create_user(Pid).


%%--------------------------------------------------------------------
%% @doc
%% Creates a new user for given room
%% @spec create_user(Room_code, User_code) -> {ok, Room_code}
%% @end
%%--------------------------------------------------------------------
create_user(Room_code, User_code) ->
    {ok, Pid} = c_room:get_room(Room_code),
    c_room:create_user(Pid, User_code).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new user for given room
%% @spec create_user(Room_code, User_code, User_nick) -> {ok, User_code}
%% @end
%%--------------------------------------------------------------------
create_user(Room_code, User_code, User_nick) ->
    {ok, Pid} = c_room:get_room(Room_code),
    c_room:create_user(Pid, User_code,User_nick).

%%--------------------------------------------------------------------
%% @doc
%% Gets pid of given user_code for given room_code
%% @spec get_user_nick(Room_code, User_code) -> {ok, User_nick}
%% @end
%%--------------------------------------------------------------------
get_user_nick(Room_code, User_code) ->
    {ok, RPid} = c_room:get_room(Room_code),
    {ok, UPid} = c_room:get_user(RPid, User_code),
    c_user:get_nick(UPid).


%%--------------------------------------------------------------------
%% @doc
%% Deletes given user from given room
%% @spec archive_user(Room_code, User_code) -> {ok, User_nick}
%% @end
%%--------------------------------------------------------------------
delete_user(Room_code, User_code) ->
    {ok, RPid} = c_room:get_room(Room_code),
    {ok, UPid} = c_room:get_user(RPid, User_code),
    c_room:delete_user(RPid,UPid).

    
%%--------------------------------------------------------------------
%% @doc
%% Gets usercount_for given room_code
%% @spec get_user_count(Room_code) -> {ok, User_count}
%% @end
%%--------------------------------------------------------------------
get_user_count(Room_code) ->
    {ok, RPid} = c_room:get_room(Room_code),
    c_room:get_user_count(RPid).


%%--------------------------------------------------------------------
%% @doc
%% Sends given message to given room by given user
%% @spec archive_user(Room_code, User_code) -> {ok, User_nick}
%% @end
%%--------------------------------------------------------------------
-spec send_message(integer(), integer(), nonempty_string()) -> ok.
send_message(Room_code, User_code, Message) ->
    {ok, RPid} = c_room:get_room(Room_code),
    {ok, UPid} = c_room:get_user(RPid, User_code),
    c_room:send_message(RPid, UPid, Message).
%%%===================================================================
%%% Internal functions
%%%===================================================================
