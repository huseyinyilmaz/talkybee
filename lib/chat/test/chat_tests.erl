-module(chat_tests).
-include_lib("eunit/include/eunit.hrl").
-include("records.hrl").

create_room_test_() ->
    {setup,
     fun fixtures:setup_chat/0,
     fun fixtures:cleanup_chat/1,
     ?_test(
	begin
	    {ok, Room_code1} = chat:create_room(),
	    {ok, 1} = chat:get_room_count(),
	    {ok, 1} = chat:create_room(1),
	    {error, already_exists} = chat:create_room(1),
	    {ok, 2} = chat:get_room_count(),
	    chat:stop_room(1),
	    chat:stop_room(Room_code1),
	    %% stop_room works with cast
	    timer:sleep(20),
	    {ok, 0} = chat:get_room_count(),
	    {error, not_found} = chat:stop_room("invalid_name")
	end)}.
     
create_user_test_() ->
    {setup,
     fun fixtures:setup_chat/0,
     fun fixtures:cleanup_chat/1,
     ?_test(
	begin
	    {ok, Room_code} = chat:create_room(),
	    {ok, 0} = chat:get_user_count(Room_code),
	    {ok, User_code} = chat:create_user(Room_code),
	    {ok, 1} = chat:get_user_count(Room_code),
	    ok = chat:delete_user(Room_code, User_code),
	    %% stop_room works with cast
	    timer:sleep(20),
	    {ok, 0} = chat:get_user_count(Room_code)
	end)}.


message_test_() ->
    {setup,
     fun fixtures:setup/0,
     fun fixtures:cleanup/1,
     ?_test(
	begin
	    {ok, Room_code} = chat:create_room(),
	    {ok, Other_room_code} = chat:create_room(),
	    {ok, User_code} = chat:create_user(Room_code),
	    {ok, Other_user_code} = chat:create_user(Other_room_code),
	    ok = chat:send_message(Room_code, User_code, <<"1">>),
	    ok = chat:send_message(Room_code, User_code, <<"2">>),
	    ok = chat:send_message(Room_code, User_code, <<"3">>),
	    ok = chat:send_message(Room_code, User_code, <<"4">>),
	    ok = chat:send_message(Room_code, User_code, <<"5">>),
	    ok = chat:send_message(Other_room_code, Other_user_code, <<"13">>), % another room

	    User_nick = ?DEFAULT_NICK,
	    
	    All_messages = [
			    #message{room_code=Room_code,
				     message_code=1,
				    user_code=User_code,
				    user_nick=User_nick,
				    content= <<"1">>},
			    #message{room_code=Room_code,
				     message_code=2,
				    user_code=User_code,
				    user_nick=User_nick,
				    content= <<"2">>},
			    #message{room_code=Room_code,
				     message_code=3,
				    user_code=User_code,
				    user_nick=User_nick,
				    content= <<"3">>},
			    #message{room_code=Room_code,
				     message_code=4,
				     user_code=User_code,
				     user_nick=User_nick,
				    content= <<"4">>},
			    #message{room_code=Room_code,
				     message_code=5,
				     user_code=User_code,
				     user_nick=User_nick,
				     content= <<"5">>}
			   ],

	    ?assertEqual({ok, All_messages},
	    		 chat:get_messages(Room_code)),
	    
	    ?assertEqual({ok, lists:sublist(All_messages,3,3)}, %[3,4,5]
			 chat:get_messages(Room_code,2))
		
	end)}.
