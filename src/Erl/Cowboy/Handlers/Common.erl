-module(erl_cowboy_handlers_common@foreign).
-export([decodeReasonImpl/6, terminateResult/0]).

decodeReasonImpl(Normal, Crash, Error, Exit, Throw, Reason) -> 
    case Reason of
        normal -> Normal;
        { crash, error, _ } -> Crash(Error);
        { crash, exit, _ } -> Crash(Exit);
        { crash, throw, _ } -> Crash(Throw)
    end.

terminateResult() -> ok.