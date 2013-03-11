-module (process_mock).
-export ([make_message_receiver/2, process_mock/2, receive_message/1]).

receive_message(Id) ->
	receive
		{mock_received, Id, Msg} ->
			{ok, Msg}
		after 5000 ->
			{error, no_message_received}
	end.




make_message_receiver(Parent_pid, Id) ->
	spawn(message_mock, process_mock,[Parent_pid, Id]).

process_mock(Parent_pid, Id) ->
	receive
		exit ->
			ok;
		Msg ->
			Parent_pid ! {mock_received, Id, Msg},
			process_mock(Parent_pid, Id)
	end.