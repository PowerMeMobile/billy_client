-module(tc).

-compile(export_all).

start_sess() ->
	{ok, Sock} = gen_tcp:connect("127.0.0.1", 16062, [binary, {active, false}]),
	billy_client_session:start(Sock).

bind_sess(Sess) ->
	gen_billy_session_c:reply_bind(Sess, [
		{client_id, <<"client1">>},
		{client_pw, <<"secureme!">>}
	]).






