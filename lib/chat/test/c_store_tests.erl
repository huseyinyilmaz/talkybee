-module(c_store_tests).
-include("records.hrl").
-include_lib("eunit/include/eunit.hrl").

create_code_test_() ->
    {setup,
     fun fixtures:setup_store/0,
     fun fixtures:cleanup_store/1,
     [?_test(
	begin
	    ?assertEqual({ok, 1}, c_store:get_next_room_code()),
	    ?assertEqual({ok, 2}, c_store:get_next_room_code()),
	    ?assertEqual({ok, 3}, c_store:get_next_room_code())
	end),
     ?_test(
	begin
	    ?assertEqual({ok, 1}, c_store:get_next_user_code()),
	    ?assertEqual({ok, 2}, c_store:get_next_user_code()),
	    ?assertEqual({ok, 3}, c_store:get_next_user_code())
	end),
     ?_test(
	begin
	    ?assertEqual({ok, 1}, c_store:get_next_message_code()),
	    ?assertEqual({ok, 2}, c_store:get_next_message_code()),
	    ?assertEqual({ok, 3}, c_store:get_next_message_code())
	end),
     ?_test(
	begin
	    Room_code = 1,
	    User_code = 2,
	    User_nick = <<"user_nick">>,
	    {ok, Message_code_1} = c_store:insert_message(Room_code,
							  User_code,
							  User_nick,
							  <<"1">>),
	    {ok, Message_code_2} = c_store:insert_message(Room_code,
							  User_code,
							  User_nick,
							  <<"2">>),
	    {ok, Message_code_3} = c_store:insert_message(Room_code,
							  User_code,
							  User_nick,
							  <<"3">>),
	    {ok, Message_code_4} = c_store:insert_message(Room_code,
							  User_code,
							  User_nick,
							  <<"4">>),
	    {ok, Message_code_5} = c_store:insert_message(Room_code,
							  User_code,
							  User_nick,
							  <<"5">>),
	    c_store:insert_message(13,User_code,User_nick,<<"13">>), %Another room
	    
	    All_messages = [
			    #message{room_code=Room_code,
				     message_code=Message_code_1,
				    user_code=User_code,
				    user_nick=User_nick,
				    content= <<"1">>},
			    #message{room_code=Room_code,
				     message_code=Message_code_2,
				    user_code=User_code,
				    user_nick=User_nick,
				    content= <<"2">>},
			    #message{room_code=Room_code,
				     message_code=Message_code_3,
				    user_code=User_code,
				    user_nick=User_nick,
				    content= <<"3">>},
			    #message{room_code=Room_code,
				     message_code=Message_code_4,
				     user_code=User_code,
				     user_nick=User_nick,
				    content= <<"4">>},
			    #message{room_code=Room_code,
				     message_code=Message_code_5,
				    user_code=User_code,
				    user_nick=User_nick,
				    content= <<"5">>}
			   ],
	    %% Check if all messages for given Room is coming
	    ?assertEqual({ok, All_messages}, c_store:get_messages(Room_code)),
	    %% Check if messages after given number for given room is coming
	    ?assertEqual({ok, lists:sublist(All_messages,3,3)}, %[3,4,5]
			 c_store:get_messages(Room_code, Message_code_2))

	end)
     ]}.



