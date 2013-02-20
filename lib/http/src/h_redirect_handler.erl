-module(h_redirect_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, State) ->
    {Host_bin,Req2} = cowboy_req:host_url(Req),
    {ok, Room_code} = chat:create_room(),
    Room_code_bin = list_to_binary(
		      integer_to_list(Room_code)),
    Url_bin = <<Host_bin/binary, <<"/rooms/">>/binary, Room_code_bin/binary>>,
    {ok, Req3} = cowboy_req:reply(302,
				  [{<<"location">>, Url_bin}],
				  <<>>,
				  Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
	ok.
