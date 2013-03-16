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
-export([handle_request/3]).

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
	       Req, State)->
    error_logger:info_report({client_user_nick, Client_user_nick}),
    case Client_room_code of
	<<>> -> {ok, Room_code} = chat:create_room();
	Room_code -> chat:create_room(Room_code)
    end,
    %% Stop old user
    case Client_user_code of
	<<>> -> ok;
	_ ->
	    %% Stop old user
	    error_logger:info_report({stop_old_user, Client_user_code}),
	    c_chat:stop_user(Client_user_code)
    end,
    % Start a new user
    {ok, User_code} =
	case Client_user_nick of
	    <<>> -> chat:create_user(self());
	    _ ->
		error_logger:info_report({create_user_with_nick,
					  Client_user_nick}),
		chat:create_user(self(), Client_user_nick)
	end,

    {ok, User_nick} = chat:get_user_nick(User_code),

    Raw_result = {[{<<"type">>,<<"connected_to_room">>},
			    {<<"room_code">>, Room_code},
			    {<<"user_code">>, User_code},
			    {<<"user_nick">>, User_nick}
			   ]},
    error_logger:info_report({raw_result, Raw_result}),
    Result = jiffy:encode({[{<<"type">>,<<"connected_to_room">>},
			    {<<"room_code">>, Room_code},
			    {<<"user_code">>, User_code},
			    {<<"user_nick">>, User_nick}
			   ]}),
    {reply, Result, Req, State};

handle_request({[{<<"type">>,<<"heartbeat">>},
		 {<<"value">>, <<"ping">>}]},
	       Req, State)->
    Result = jiffy:encode({[{<<"type">>,<<"heartbeat">>},
			    {<<"value">>, <<"pong">>}]}),
    {reply, Result, Req, State};


handle_request(Msg, Req, State)->
    Result=jiffy:encode({[{<<"type">>,<<"unhandled_msg">>},{<<"msg">>, Msg}]}),
    {reply, Result, Req, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

