%% Event types that room event handler uses
-record(user_data, {code, nick}).
-record(message, {code, message}).
-record(user_removed,{code}).

%% filtered by user this type does not sent to client
-record(user_handshake,{pid}).

%%--------------------------------------------------------------------
%% @doc
%% Converts messages to structures that can be serialized to json by jiffy
%% @end
%%--------------------------------------------------------------------

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


    
