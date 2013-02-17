%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%%
%%% @end
%%% Created : 22 Jan 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(c_user).

-behaviour(gen_server).
-include_lib("eunit/include/eunit.hrl").

%% API
-export([start_link/3, get_nick/1, get_code/1, get_info/1,
	delete/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {room, code, nick}).

%%%===================================================================
%%% API
%%%===================================================================

-spec get_nick(pid()) -> {ok,nonempty_string()}.
get_nick(Pid) ->
    gen_server:call(Pid,get_nick).


-spec get_code(pid()) -> {ok,integer()}.
get_code(Pid) ->
    gen_server:call(Pid,get_code).

-spec get_info(pid()) -> {ok,tuple()}.
get_info(Pid)->
    gen_server:call(Pid,get_info).

-spec delete(pid()) -> ok.
delete(Pid) ->
    gen_server:cast(Pid,delete),
    ok.
%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link(Code, Nick) -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Room_pid, Code, Nick) ->
    gen_server:start_link(?MODULE, [Room_pid, Code, Nick], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

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
init([Room_pid, Code, Nick]) ->
    {ok, #state{room=Room_pid, code=Code,nick=Nick}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(get_code, _From, #state{code=Code}=State) ->
    Reply = {ok, Code},
    {reply, Reply, State};

handle_call(get_nick, _From, #state{nick=Nick}=State) ->
    Reply = {ok, Nick},
    {reply, Reply, State};

handle_call(get_info, _From, #state{room=Room_pid,
				    code=Code,
				    nick=Nick}=State) ->
    Reply = {ok, {Room_pid, Code, Nick}},
    {reply, Reply, State}.

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
handle_cast(delete, State) ->
    {stop, normal, State}.

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


%%%===================================================================
%%% Tests
%%%===================================================================
get_data_test() ->
    Nick = "test",
    ok = chat:start(),
    {ok, Room} = chat:create_room(),
    {ok, User} = chat:create_user(Room,1,Nick),
    {ok, RPid} = c_room:get_room(Room),
    {ok, UPid} = c_room:get_user(RPid, User),
    true = is_process_alive(UPid),
    {ok, User} = c_user:get_code(UPid),
    {ok, Nick} = c_user:get_nick(UPid),
    {ok, {RPid, User, Nick}} = c_user:get_info(UPid),
    ok = chat:stop().
    
