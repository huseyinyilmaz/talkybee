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
	 create_user/1, create_user/2, create_user/3, stop_user/1,
	 get_room_count/0, get_user_nick/1, get_user_count/0, add_user/2,
	 remove_user/2, send_message/3, pop_messages/1, introduce_user/2,
	 set_user_nick/2, publish_user/2, lock_room/1, unlock_room/1]).

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
	{error, Error} -> {stop, Error}
    end.

%%--------------------------------------------------------------------
%% @doc
%% Creates a new user
%% @spec create_user(Handler) -> {ok, Room_code}
%% @end
%%--------------------------------------------------------------------
create_user(Handler)->
    Code = c_utils:generate_code(),
    create_user(Handler, Code, Code).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new user
%% @spec create_user(Handler) -> {ok, Room_code}
%% @end
%%--------------------------------------------------------------------
create_user(Handler, Nick)->
    Code = c_utils:generate_code(),
    create_user(Handler, Code, Nick).

%%--------------------------------------------------------------------
%% @doc
%% Creates a new room with given Room_code
%% @spec create_room(Room_code) -> {ok, Room_code}
%% @end
%%--------------------------------------------------------------------
create_user(Handler, Code, Nick) ->
    case c_user_sup:start_child(Handler, Code, Nick) of
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
    error_logger:info_report({chat__stop_user, Code}),
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


%%--------------------------------------------------------------------
%% @doc
%% Adds given user to given room
%% @spec get_user_nick(Room_code, User_code) -> {ok, User_nick}
%% @end
%%--------------------------------------------------------------------
add_user(Room_code, User_code) ->
    error_logger:info_report({add_user, Room_code, c_room:get_room(Room_code)}),
    case c_room:get_room(Room_code) of
	{ok, Rpid} ->
	    case c_user:get_user(User_code) of
		{ok, Upid} -> c_room:add_user(Rpid, Upid);
		Error -> Error
	    end;
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Removes given user from given room
%% @spec remove_user_nick(Room_code, User_code) -> {ok, User_nick}
%% @end
%%--------------------------------------------------------------------
remove_user(Room_code, User_code) ->
    error_logger:info_report({remove_user_chat}),
    case c_room:get_room(Room_code) of
	{ok, Rpid} ->
	    case c_user:get_user(User_code) of
		{ok, Upid} -> c_room:remove_user(Rpid, Upid);
		Error -> Error
	    end;
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Sends info request to all users in the Room with users
%% own information.
%% All users will return their info to given user
%% @end
%%--------------------------------------------------------------------
-spec introduce_user(binary(), binary()) -> ok.
introduce_user(Room_code, User_code) ->
    case c_room:get_room(Room_code) of
	{ok, Rpid} ->
	    case c_user:get_user(User_code) of
		{ok, Upid} -> c_room:introduce_user(Rpid, Upid);
		Error -> Error
	    end;
	Error -> Error
    end.

%%--------------------------------------------------------------------
%% @doc
%% Sends given room a message by given user
%% @spec get_user_nick(Room_code, User_code) -> {ok, User_nick}
%% @end
%%--------------------------------------------------------------------
send_message(Room_code, User_code, Message) ->
	{ok, Rpid} = c_room:get_room(Room_code),
	c_room:send_message(Rpid, User_code, Message).

%%--------------------------------------------------------------------
%% @doc
%% Gets given users messages
%% @spec get_user_nick(Room_code, User_code) -> {ok, User_nick}
%% @end
%%--------------------------------------------------------------------
pop_messages(User_code) ->
    {ok, Upid} = c_user:get_user(User_code),
    c_user:pop_messages(Upid).

set_user_nick(Code, Nick) ->
    {ok, Pid} = c_user:get_user(Code),
    c_user:set_nick(Pid, Nick).

publish_user(Room_code, User_code) ->
    case c_room:get_room(Room_code) of
	{ok, Rpid} ->
	    case c_user:get_user(User_code) of
		{ok, Upid} -> c_room:publish_user(Rpid, Upid);
		Error -> Error
	    end;
	Error -> Error
    end.
    

%%--------------------------------------------------------------------
%% @doc
%% Locks room with given code
%% @end
%%--------------------------------------------------------------------
lock_room(Room_code) ->
	{ok, Rpid} = c_room:get_room(Room_code),
	c_room:lock(Rpid).

%%--------------------------------------------------------------------
%% @doc
%% Unlocks room with given code
%% @end
%%--------------------------------------------------------------------
unlock_room(Room_code) ->
	{ok, Rpid} = c_room:get_room(Room_code),
	c_room:unlock(Rpid).
