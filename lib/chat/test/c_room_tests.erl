-module(c_room_tests).
-include_lib("eunit/include/eunit.hrl").

get_room_get_code_test_() ->
    {setup,
     fun fixtures:setup_chat/0,
     fun fixtures:cleanup_chat/1,
     ?_test(
	begin
	    Code = 1,
	    {ok, Room} = c_room:start_link(Code),
	    ?assertEqual({ok, 1}, c_room:get_code(Room)),
	    ?assertEqual({ok, Room}, c_room:get_room(1))
	end)}.

get_dead_room_code_test_() ->
    {setup,
     fun fixtures:setup_chat/0,
     fun fixtures:cleanup_chat/1,
     ?_test(
	begin
	    Pid = spawn(timer, sleep,[0]),
	    timer:sleep(10),
	    ets:insert(rooms, {invalid, Pid}),
	    ?assertEqual({ok, 1},c_room:get_room_count()),
	    ?assertEqual({error, not_found},c_room:get_room(invalid)),
	    ?assertEqual({ok, 0}, c_room:get_room_count()),
	    ?assertEqual({error, not_found}, c_room:get_room(invalid))
	end)}.

get_user_test_() ->
    {setup,
     fun fixtures:setup_chat/0,
     fun fixtures:cleanup_chat/1,
     ?_test(
	begin
	    {ok, Room} = chat:create_room(),
	    {ok, User} = chat:create_user(Room),
	    {ok, RPid} = c_room:get_room(Room),
	    {ok, UPid} = c_room:get_user(RPid, User),
	    ?assertEqual(true, is_process_alive(UPid))
	end)}.
