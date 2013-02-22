-module(fixtures).
-export([setup_chat/0, cleanup_chat/1
	]).

setup_chat() ->
    ok = chat:start().

cleanup_chat(_) ->
    ok = chat:stop().
