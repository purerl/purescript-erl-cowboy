-module(erl_cowboy_routes@foreign).
-export([compile/1,string/1]).

compile(Routes) -> cowboy_router:compile(Routes).
string(S) -> unicode:characters_to_list(S, utf8).
