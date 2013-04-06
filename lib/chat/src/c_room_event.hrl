%% Event types that room event handler uses
-record(user_data, {code, nick}).
-record(room_data, {code, is_locked}).
-record(message, {code, message}).
-record(user_removed,{code}).
-record(error, {code, message}).

%% filtered by user this type does not sent to client
-record(user_handshake,{pid}).



    
