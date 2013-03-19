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

%% API
-export([start_link/1, get_code/1, get_room/1,
	 get_event_manager/1, get_count/0, stop/1,
	 add_user/2, remove_user/2, broadcast/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {code,
		event_manager}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Supervisor callback that creates an instance
%% @end
%%--------------------------------------------------------------------
-spec start_link(binary()) -> {ok, pid()} | ignore | {error, any()}.
start_link(Code) ->
    gen_server:start_link(?MODULE, [Code], []).

%%--------------------------------------------------------------------
%% @doc
%% Gets room code from pid
%% @end
%%--------------------------------------------------------------------
-spec get_code(pid()) -> {ok, integer()}.
get_code(Pid) ->
    gen_server:call(Pid, get_code).

%%--------------------------------------------------------------------
%% @doc
%% Gets room code from pid
%% @end
%%--------------------------------------------------------------------
-spec get_event_manager(pid()) -> {ok, pid()}.
get_event_manager(Pid) ->
    gen_server:call(Pid, get_event_manager).

get_room(Code) ->    
    case ets:lookup(rooms, Code) of
        [{Code, Pid}] ->
	    %% if process is dead remove it and
	    %% return not found
	    case is_process_alive(Pid) of
		true -> {ok, Pid};
		false -> ets:delete(rooms, Code),
			 {stop, not_found}
	    end;
        []           -> {stop, not_found}
    end.

stop(Pid) ->
    gen_server:cast(Pid, stop).

-spec get_count() -> {ok, integer()}.
get_count() ->
    {ok, ets:info(rooms, size)}.

add_user(Rpid, Upid) ->
    ok = gen_server:call(Rpid,{add_user, Upid}),
    ok = gen_server:cast(Rpid, {introduce_user, Upid}).

remove_user(Rpid, Upid) ->
    gen_server:call(Rpid,{remove_user, Upid}).
    
broadcast(Rpid, Message) ->
    gen_server:cast(Rpid, {broadcast, Message}).

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
init([Code]) ->
    case get_room(Code) of
	{ok , _} -> {stop, already_exists};
	{stop, not_found} ->
	    {ok, Event_manager} = c_room_event:start_link(),
	    ets:insert(rooms, {Code, self()}),
	    {ok, #state{code=Code,
			event_manager=Event_manager}}
    end.



handle_call(get_code, _From, #state{code=Code}=State) ->
    {reply, {ok, Code}, State};

handle_call(get_event_manager, _From, #state{event_manager=Event_manager}=State) ->
    {reply, {ok, Event_manager}, State};

handle_call({add_user, Upid}, _From, #state{event_manager=Event_manager}=State) ->
    c_room_event:add_handler(Event_manager, Upid),
    gen_event:notify(Event_manager, {introduce_user, Upid}),
    {reply, ok, State};

handle_call({remove_user, Upid}, _From, #state{event_manager=Event_manager}=State) ->
    {ok, Code} = c_user:get_code(Upid),
    gen_event:notify(Event_manager,{user_removed, Code}),
    c_room_event:delete_handler(Event_manager, Upid),
    {reply, {ok, Event_manager}, State}.

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
handle_cast({broadcast, Message}, #state{event_manager=Event_manager}=State) ->
    gen_event:notify(Event_manager, {Message}),
    {noreply, State};
handle_cast(stop, State) ->
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
