%%%-------------------------------------------------------------------
%%% @author Huseyin Yilmaz <huseyin@huseyin-work>
%%% @copyright (C) 2013, Huseyin Yilmaz
%%% @doc
%%% Chat store module provieds api for handling persistence for chat app
%%% @end
%%% Created :  6 Feb 2013 by Huseyin Yilmaz <huseyin@huseyin-work>
%%%-------------------------------------------------------------------
-module(c_store).

-include("records.hrl").

%% API
-export([init/0, init/1, insert_message/4, get_messages/1,
	 get_messages/2, get_next_room_code/0,
	 get_next_user_code/0, get_next_message_code/0]).



%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% initialize mnesia tables.
%% default initialization is for development.
%% @end
%%--------------------------------------------------------------------
-spec init() -> ok.
init() ->
    init(development).

%%--------------------------------------------------------------------
%% @doc
%% initialize mnesia tables for given type
%% Type can be production of development symbol
%% @end
%%--------------------------------------------------------------------
-spec init(development | production) -> ok.
init(Init_type) ->
    % ensure that /tmp/mnesia directory is present
    ok = filelib:ensure_dir("/tmp/mnesia/ensure.txt"),
    stopped = mnesia:stop(),
    ok = mnesia:delete_schema([node()]),
    ok = mnesia:create_schema([node()]),
    ok = mnesia:start(),
    ok = init_tables(Init_type).

insert_message(Room_code, User_code, User_nick, Content) ->
    {ok, Message_code} = get_next_message_code(),
    Msg = #message{room_code=Room_code,
		   message_code = Message_code,
		   user_code=User_code,
		   user_nick=User_nick,
		   content=Content},
    {atomic, ok} = mnesia:transaction(fun() -> mnesia:write(Msg) end),
    {ok, Message_code}.


get_messages(Room_code) ->
    {ok, mnesia:dirty_read(message, Room_code)}.
    
get_messages(Room_code, Last_msg_code) ->
    Match = #message{room_code=Room_code,
		     message_code='$1',
		     user_code='_',
		     user_nick='_',
		     content='_'},
    Guards = [{'<', Last_msg_code, '$1'}],
    Result = ['$_'],
    {ok, mnesia:dirty_select(message,[{Match, Guards,Result}])}.
%%--------------------------------------------------------------------
%% @doc
%% initialize mnesia tables for given type
%% Type can be production of development symbol
%% Warning: Those codes might not be unique in multi node environment
%% @end
%%--------------------------------------------------------------------
get_next_room_code() ->
    {ok, mnesia:dirty_update_counter(counter, room, 1)}.

get_next_user_code() ->
    {ok , mnesia:dirty_update_counter(counter, user, 1)}.

get_next_message_code() ->
    {ok, mnesia:dirty_update_counter(counter, message, 1)}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
production_table_specs() ->
    [[message,
      [{type, bag},
       {record_name, message},
       {disc_only_copies, [node()]},
       {attributes, record_info(fields, message)}]],
     [counter,
      [{disc_copies, [node()]},
       {attributes, record_info(fields, counter)}]]].

development_table_specs()->
    [[message,
      [{type, bag},
       {record_name, message},
       {attributes, record_info(fields, message)}]],
     [counter,
     [{attributes, record_info(fields, counter)}]]].
    
-spec init_tables(development | production) -> ok.
init_tables(Init_type) ->
    Specs = case Init_type of
		development -> development_table_specs();
		production -> production_table_specs()
	    end,
    lists:map(fun(Spec)->
		      {atomic, ok} = apply(mnesia, create_table, Spec)
	      end, Specs),
    ok.

