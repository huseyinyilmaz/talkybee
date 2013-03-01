
%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Debugging code 
%%% @end
%%% Created : 21 Feb 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(debug).

%% API
-export([init/0, start/0, stop/0, restart/0,
	start_dev/0, stop_dev/0, restart_dev/0]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @docn
%% @spec
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    io:format("Initializing environment", []),
    application:start(sasl),    
    appmon:start(),
    code:add_patha("deps/ranch/ebin"),
    code:add_patha("deps/cowboy/ebin"),
    code:add_patha("deps/bullet/ebin"),
    code:add_patha("deps/jiffy/ebin"),
    code:add_patha("deps/mimetypes/ebin"),
    code:add_patha("lib/http/ebin"),
    code:add_patha("lib/chat/ebin"),
    ok.

-spec start() -> ok.
start() ->
    io:format("Starting apps", []),
    ok = chat:start(),
    ok = http:start(),
    ok.

-spec stop() -> ok.
stop() ->
    chat:stop(),
    http:stop(),
    ok.

-spec restart() -> ok.
restart() ->
    stop(),
    start().


start_dev() ->
    Room_code = <<"room">>,
    User1_code = <<"user1">>,
    User2_code = <<"user2">>,
	
    {ok, Room_code} = chat:create_room(Room_code),
    {ok, User1_code} = chat:create_user(User1_code, User1_code),
    {ok, User2_code} = chat:create_user(User2_code, User2_code),
    chat:add_user(Room_code,User1_code),
    chat:add_user(Room_code,User2_code),
    {Room_code, User1_code, User2_code}.


stop_dev() ->
    Room_code = <<"room">>,
    User1_code = <<"user1">>,
    User2_code = <<"user2">>,
    
    chat:stop_user(User1_code),
    chat:stop_user(User2_code),
    chat:stop_room(Room_code).

restart_dev() ->
    ok = stop_dev(),
    start_dev().
%%%===================================================================
%%% Internal functions
%%%===================================================================
