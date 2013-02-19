%% Table records

-define(DEFAULT_NICK, <<"Anonymous">>). 
-record(message, {room_code, message_code, user_code, user_nick, content}).
-record(counter, {key, value}).
