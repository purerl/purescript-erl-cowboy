-module(erl_cowboy_handlers_loop@foreign).
-export([initResult/2, hibernate/2, continueHibernate/2, continue/2, stop/2]).
initResult(S, Req) -> {cowboy_loop, Req, S}.
hibernate(S, Req) -> {cowboy_loop, Req, S, hibernate}.

continue(S, Req) -> {ok, Req, S}.
continueHibernate(S, Req) -> {ok, Req, S, hibernate}.
stop(S, Req) -> {stop, Req, S}.