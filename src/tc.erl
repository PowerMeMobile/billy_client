-module(tc).

-compile(export_all).

start_sess1() ->
	billy_client_session:start_link("localhost", 16062, <<"client1">>, <<"secureme!">>).

unbind_sess(Sess) ->
	billy_client_session:unbind_async(Sess).

