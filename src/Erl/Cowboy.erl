-module(erl_cowboy@foreign).
-export([startClear/3, protocolOpts/1, env/1]).

startClear(Name, TransOpts, ProtoOpts) -> fun () ->
  cowboy:start_clear(Name, TransOpts, ProtoOpts)
end.

protocolOpts(Opts) ->
  lists:foldl(
    fun ({env, E}, M) -> M#{env => E};
        ({middlewares, X}, M) -> M#{middlewares => X};
        (_, M) -> M
    end,
    #{},
    Opts
  ).

env(Env) ->
  lists:foldl(
    fun ({dispatch, X}, M) -> M#{dispatch => X};
        ({fn, X}, M) -> M#{fn => X};
        (_, M) -> M
    end,
    #{},
    Env
  ).