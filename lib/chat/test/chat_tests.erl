-module(chat_tests).
-include_lib("eunit/include/eunit.hrl").

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

