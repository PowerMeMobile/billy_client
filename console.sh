#!/bin/bash

erl -pa ebin/ deps/*/ebin/ -eval "
lists:foreach(fun application:start/1, [

gproc, piqi, compiler, syntax_tools, lager,
uuid, sync, billy_common, billy_session_c, billy_client

]),
sync:growl(false)."
