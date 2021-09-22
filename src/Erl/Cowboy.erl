-module(erl_cowboy@foreign).
-export([startClear_/3, startTls_/3, stopListener/1]).

startClear_(Name, TransOpts, ProtoOpts) ->
  fun () ->
      case cowboy:start_clear(Name, TransOpts, ProtoOpts) of
        {ok, _Pid} -> {right, unit};
        {error, Reason} -> {left, Reason}
      end
  end.

startTls_(Name, TransOpts, ProtoOpts) ->
  fun () ->
      case cowboy:start_tls(Name, TransOpts, ProtoOpts) of
        {ok, _Pid} -> {right, unit};
        {error, Reason} -> {left, Reason}
      end
  end.

stopListener(Name) ->
  fun() ->
      cowboy:stop_listener(Name),
      unit
  end.
