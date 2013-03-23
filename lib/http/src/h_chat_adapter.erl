%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Adapter
%%% @end
%%% Created : 14 Mar 2013 by Huseyin Yilmaz <>
%%%-------------------------------------------------------------------
-module(h_chat_adapter).

%% API
-export([handle_request/3, handle_info/3,
	 terminate/1, initial_state/0]).

-include("../../chat/src/c_room_event.hrl").

-record(state,{room_code, user_code}).

%%--------------------------------------------------------------------
%% @doc
%% Return initial state for handlers
%% @end
%%--------------------------------------------------------------------
-spec initial_state() -> tuple().
initial_state()->
    #state{room_code=undefined,
	   user_code=undefined}.

%%--------------------------------------------------------------------
%% @doc
%% Converts messages to structures that can be serialized to json by jiffy
%% @end
%%--------------------------------------------------------------------
-spec message_to_jiffy(tuple()) -> tuple().
message_to_jiffy(#user_data{code=Code,nick=Nick}) ->
    {[{<<"type">>,<<"user_data">>},
      {<<"code">>, Code},
      {<<"nick">>, Nick}
     ]};

message_to_jiffy(#message{code=Code,message=Message}) ->
    {[{<<"type">>,<<"message">>},
      {<<"code">>, Code},
      {<<"message">>, Message}
     ]};

message_to_jiffy(#user_removed{code=Code}) ->
    {[{<<"type">>,<<"user_removed">>},
      {<<"code">>, Code}
     ]}.


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------
handle_request({[{<<"type">>,<<"connect_to_room">>},
		 {<<"room_code">>, Client_room_code},
		 {<<"user_code">>, Client_user_code},
		 {<<"user_nick">>, Client_user_nick}]},
	       Req, _State)->
    %% TODO We should make sure that if _State is available
    %% it is same with Client_room_code
    error_logger:info_report({client_user_nick, Client_user_nick}),
    %% Make sure that a room is ready for this session
    %% {ok, Code}|{error, already_exists}
    case Client_room_code of
	<<>> -> {ok, Room_code} = chat:create_room();
	Room_code -> chat:create_room(Room_code)
    end,
    
    %% Stop old user
    %% ok , {error, not found}
    case Client_user_code of
	<<>> -> ok;
	_ ->
	    stop_user(Client_room_code, Client_user_code)
    end,
    
    %% Start a new user
    %% Old user is already removed. Create a new one
    {ok, User_code} =
	case Client_user_nick of
	    <<>> -> chat:create_user(self());
	    _ ->
		error_logger:info_report({create_user_with_nick,
					  Client_user_nick}),
		chat:create_user(self(), Client_user_nick)
	end,
    %% Get User nick from user
    {ok, User_nick} = chat:get_user_nick(User_code),
    %% Add User to room
    ok = chat:add_user(Room_code, User_code),
    Raw_result =[
	{[{<<"type">>,<<"connected_to_room">>},
			    {<<"room_code">>, Room_code},
			    {<<"user_code">>, User_code},
			    {<<"user_nick">>, User_nick}
			   ]}
		],
    error_logger:info_report({raw_result, Raw_result}),
    Result = jiffy:encode(Raw_result),
    {reply, Result, Req, #state{user_code=User_code, room_code=Room_code}};

handle_request({[{<<"type">>,<<"heartbeat">>},
		 {<<"value">>, <<"ping">>}]},
	       Req, State)->
    Result = jiffy:encode([{[{<<"type">>,<<"heartbeat">>},
			    {<<"value">>, <<"pong">>}]}]),
    {reply, Result, Req, State};

handle_request({[{<<"type">>,<<"message">>},
		 {<<"value">>, Message}]},
	       Req, #state{room_code=Room_code, user_code=User_code}=State)->
    chat:send_message(Room_code,User_code,Message),
    {ok, Req, State};

handle_request({[{<<"type">>,<<"rename">>},
		 {<<"value">>, Nick}]},
	       Req, #state{room_code=Room_code, user_code=User_code}=State)->
    chat:set_user_nick(User_code, Nick),
    %% publish renamed user name
    chat:publish_user(Room_code, User_code),
    {ok, Req, State};

handle_request(Msg, Req, State)->
    Result=jiffy:encode({[{<<"type">>,<<"unhandled_msg">>},{<<"msg">>, Msg}]}),
    {reply, Result, Req, State}.

handle_info({have_message, Pid}, Req, State) ->
    {ok, Messages} = c_user:pop_messages(Pid),
    error_logger:info_report({have_message, Messages}),
    Message_list = lists:map(fun message_to_jiffy/1, Messages),
    Result = jiffy:encode(Message_list),
    {reply, Result, Req, State}.

terminate(#state{user_code=undefined}) -> ok;
terminate(#state{room_code=undefined}) -> ok;
terminate(#state{user_code=User_code, room_code=Room_code}) ->
    stop_user(Room_code, User_code).

stop_user(Room_code, User_code) ->
    error_logger:info_report({stop_old_user, User_code}),
    chat:remove_user(Room_code, User_code),
    chat:stop_user(User_code).


