
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
-export([init/0, start/0, stop/0, restart/0, create_structures/0]).

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
    sync:go(),
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

create_structures() ->
    chat:create_room(1),
    {ok, Room} = c_room:get_room(1),
    chat:create_user(1,1),
    chat:create_user(2,2),
    {ok, User1} = c_user:get_user(1),
    {ok, User2} = c_user:get_user(2),
    c_room:add_user(Room,User1),
    c_room:add_user(Room,User2),
    c_room:send_message(Room,User1,<<"debug test---------------">>),
    {Room, User1, User2}.
    
%%%===================================================================
%%% Internal functions
%%%===================================================================
