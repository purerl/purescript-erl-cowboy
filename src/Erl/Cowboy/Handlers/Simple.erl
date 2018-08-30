-module(erl_cowboy_handlers_simple@foreign).
-export([initResult/2]).

initResult(S, Req) -> {ok, Req, S}.


