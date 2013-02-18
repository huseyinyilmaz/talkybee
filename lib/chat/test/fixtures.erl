-module(fixtures).
-export([setup_chat/0, cleanup_chat/1,
	 setup_store/0, cleanup_store/1,
	 setup/0, cleanup/1
	]).

setup_chat() ->
    ok = chat:start().

cleanup_chat(_) ->
    ok = chat:stop().
    
setup_store() ->
    ok = c_store:init(development).

cleanup_store(_) ->
    ok.

setup() ->
    ok = setup_store(),
    ok = setup_chat().

cleanup(Config) ->
    ok = cleanup_store(Config),
    ok = cleanup_chat(Config).
    
    
    
