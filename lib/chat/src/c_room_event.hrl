%% Event types that room event handler uses
-record(user_data, {code, nick}).
-record(message, {code, message}).
-record(user_removed,{code}).

%% filtered by user this type does not sent to client
-record(user_handshake,{pid}).



    
