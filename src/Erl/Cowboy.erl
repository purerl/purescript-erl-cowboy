-module(erl_cowboy@foreign).
-export([compile/1,startHttp/3,string/1,toEnvValue/1]).

compile(Routes) -> cowboy_router:compile(Routes).

toEnvValue(X) -> X.

% TODO: start_clear in later 2.0-pre versions
startHttp(NumAcceptors, TransOpts, ProtoOpts) -> fun () ->
  cowboy:start_http(the_http_listener, NumAcceptors, TransOpts, ProtoOpts) end.

string(S) -> unicode:characters_to_list(S, utf8).
