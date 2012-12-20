#!/bin/bash

erl -pa ebin/ deps/*/ebin/ -eval "
lists:foreach(fun application:start/1, [

gproc, piqi, compiler, syntax_tools, lager,
billy_common, billy_session_c, billy_client,
sync

]),

lager:set_loglevel(lager_console_backend, debug).
"
