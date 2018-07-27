-module(erl_cowboy_routes@foreign).
-export([compile/1]).

compile(Routes) -> cowboy_router:compile(Routes).
