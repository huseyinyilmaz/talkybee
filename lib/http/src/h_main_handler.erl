-module(h_main_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->

    Doc = {[
	    {foo,bar},
	    {test1,test2}
	   ]},
    Reply_text = jiffy:encode(Doc),
    {ok, Req2} = cowboy_req:reply(200,
				  [{<<"content-type">>, <<"application/json">>}],
				  Reply_text,
				  Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
	ok.
