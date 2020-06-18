-module(erl_cowboy@foreign).
-export([startClear/3, stopListener/1, protocolOpts/1, env/1]).

startClear(Name, TransOpts, ProtoOpts) ->
  fun () ->
      case cowboy:start_clear(Name, TransOpts, ProtoOpts) of
        {ok, _Pid} -> {right, unit};
        {error, Reason} -> {left, Reason}
      end
  end.

stopListener(Name) ->
  fun() ->
      cowboy:stop_listener(Name),
      unit
  end.

protocolOpts(Opts) ->
  lists:foldl(
    fun ({env, E}, M) -> M#{env => E};
        ({middlewares, X}, M) -> M#{middlewares => X};
        ({streamHandlers, X}, M) -> M#{stream_handlers => X};
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
