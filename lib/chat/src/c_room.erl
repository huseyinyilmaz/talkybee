%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 22 Jan 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(c_room).

-behaviour(gen_server).

-include("records.hrl").

%% API
-export([start_link/0, start_link/1, get_code/1,get_room/1,
	 create_user/1, create_user/2, create_user/3, get_room_count/0,
	 get_user/2, get_user_count/1, delete_user/2, send_message/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3, stop/1]).

-define(SERVER, ?MODULE). 

-record(state, {room_code,
		content,
		user_sup,
		users}).

%%%===================================================================
%%% API
%%%===================================================================

%%%===================================================================
%%% ROOM API's
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Gets room code from pid
%% @end
%%--------------------------------------------------------------------
-spec get_code(pid()) -> {ok, integer()}.
get_code(Pid) ->
    gen_server:call(Pid, get_code).

get_room(Room_code) ->    
    case ets:lookup(rooms, Room_code) of
        [{Room_code, Pid}] ->
	    %% if process is dead remove it and
	    %% return not found
	    case is_process_alive(Pid) of
		true -> {ok, Pid};
		false -> ets:delete(rooms, Room_code),
			 {error, not_found}
	    end;
        []           -> {error, not_found}
    end.

stop(Pid) ->
    gen_server:cast(Pid, stop).

-spec get_room_count() -> {ok, integer()}.
get_room_count() ->
    {ok, ets:info(rooms, size)}.

%%%===================================================================
%%% User API's
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Create user for given room
%% @end
%%--------------------------------------------------------------------
-spec create_user(pid()) -> {ok, integer()}.
create_user(Pid) ->
    User_code = erlang:phash2({node(), now()}),
    create_user(Pid, User_code).

%%--------------------------------------------------------------------
%% @doc
%% Create user for given room
%% @end
%%--------------------------------------------------------------------
-spec create_user(pid(), User_code) -> {ok, User_code} when User_code::integer().
create_user(Pid, User_code) ->
    User_nick = "Anonymous",
    create_user(Pid, User_code, User_nick).

%%--------------------------------------------------------------------
%% @doc
%% Create user for given room
%% @end
%%--------------------------------------------------------------------
-spec create_user(pid(), User_code, nonempty_string()) -> {ok, User_code}
							      when User_code::integer().
create_user(Pid, User_code, User_nick) ->
    gen_server:call(Pid, {create_user, User_code, User_nick}).

%%--------------------------------------------------------------------
%% @doc
%% get user's Pid that has given user code
%% @end
%%--------------------------------------------------------------------
-spec get_user(pid(), User_code::integer()) -> {ok, pid()} |
					       {error, Error::atom()}.
get_user(Pid, User_code) ->
    gen_server:call(Pid, {get_user, User_code}).


%%--------------------------------------------------------------------
%% @doc
%% get number of users for given room 
%% @end
%%--------------------------------------------------------------------
-spec get_user_count(pid()) -> {ok, pid()}.
get_user_count(RPid) ->
    gen_server:call(RPid, get_user_count).

%%--------------------------------------------------------------------
%% @doc
%% get number of users for given room 
%% @end
%%--------------------------------------------------------------------
-spec delete_user(pid(), pid()) -> ok | {error, Error::atom()}.
delete_user(Room_pid, User_pid) ->
    gen_server:cast(Room_pid, {delete_user, User_pid}).


%%--------------------------------------------------------------------
%% @doc
%% Sends given message to given room by given user
%% @spec archive_user(Room_code, User_code) -> {ok, User_nick}
%% @end
%%--------------------------------------------------------------------
-spec send_message(pid(), pid(), nonempty_string()) -> ok.
send_message(Room_pid, User_pid, Message) ->
    gen_server:cast(Room_pid, {send_message, User_pid, Message}).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new room server
%%
%% @spec start_link(Room_code) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Room_code) ->
    gen_server:start_link(?MODULE, [Room_code], []).

%%--------------------------------------------------------------------
%% @doc
%% Starts a new room server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    Room_code = erlang:phash2({node(), now()}),
    start_link(Room_code).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Room_code]) ->
    case get_room(Room_code) of
	{ok , _} -> {stop, already_exists};
	{error, not_found} ->
	    {ok, User_sup} = c_user_sup:start_link(),
	    ets:insert(rooms, {Room_code, self()}),
	    Users = ets:new(users,[set]),
	    {ok, #state{room_code=Room_code,
			content=queue:new(),
			user_sup=User_sup,
			users=Users}}
    end.



handle_call(get_code, _From, #state{room_code=Room_code}=State) ->
    {reply, {ok, Room_code}, State};
    
handle_call({create_user, User_code, User_nick},
	    _From,
	    #state{user_sup=SUP_Pid,users=Users}=State) ->
    %% create user
    {ok, UPid} = c_user_sup:start_child(SUP_Pid,self(), User_code, User_nick),
    {ok, User_code} = c_user:get_code(UPid),
    %% insert user pid into user_code pid mapping
    ets:insert(Users,{User_code, UPid}),
    Response = {ok, User_code},
    {reply, Response, State};

handle_call({get_user, User_code},
	    _From,
	    #state{users=Users}=State) ->
    
    case ets:lookup(Users, User_code) of
        [{User_code, UPid}] ->
	    %% if process is dead remove it and
	    %% return not found
	    case is_process_alive(UPid) of
		true ->
		    Response = {ok, UPid};
		false -> ets:delete(Users, User_code),
			 Response = {error, not_found}
	    end;
        [] ->
	    Response = {error, not_found}
    end,
    {reply, Response, State};

handle_call(get_user_count, _From, #state{users=Users}=State) ->
    Response = {ok, ets:info(Users, size)},
    {reply, Response, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast({delete_user, User_pid}, #state{users=Users}=State) ->
    ok = c_user:delete(User_pid),
    ets:match_delete(Users, {'_', User_pid}),
    {noreply, State};

handle_cast({send_message, User_pid, Content}, #state{room_code=Room_code}=State) ->
    {ok, {_, User_code, User_nick}} = c_user:get_info(User_pid),
    {ok, _} = c_store:insert_message(Room_code, User_code, User_nick, Content),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ets:match_delete(rooms, {'_', self()}),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
