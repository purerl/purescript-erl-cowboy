-module(erl_cowboy_handlers_webSocket@foreign).
-export([initResult/2, okResult/1, hibernateResult/1, replyResult/2, replyAndHibernateResult/2, stopResult/1, decodeReasonImpl/17, decodeInFrameImpl/5, encodeOutFrameImpl/1]).

okResult(S) -> {ok, S}.

hibernateResult(S) -> {ok, S, hibernate}.

replyResult(S, Frames) -> {reply, Frames, S}.

replyAndHibernateResult(S, Frames) -> {reply, Frames, S, hibernate}.

initResult(S, Req) -> {cowboy_websocket, Req, S}.

stopResult(S) -> {stop, S}.

decodeReasonImpl(Normal,
    Remote,
        Nothing,
        Just,
            RemotePayload,
    Stop,
    Timeout,
    Crash,
        CrashError,
        CrashExit,
        CrashThrow,
    Error,
        BadEncoding,
        BadFrame,
        Closed,
        OtherError,
    Reason) -> 
    case Reason of
        normal -> Normal;
        remote -> Remote(Nothing);
        {remote, Code, Payload} -> Remote(Just( (RemotePayload(Code))(Payload) ));
        stop -> Stop;
        timeout -> Timeout;
        { crash, error, _ } -> Crash(CrashError);
        { crash, exit, _ } -> Crash(CrashExit);
        { crash, throw, _ } -> Crash(CrashThrow);
        { error, badencoding } -> Error(BadEncoding);
        { error, badframe } -> Error(BadFrame);
        { error, closed } -> Error(Closed);
        { error, X } -> Error(OtherError(X))
    end.

decodeInFrameImpl(Text, Binary, Ping, Pong, Frame) ->
    case Frame of
        {text, B} -> Text(B);
        {binary, B} -> Binary(B);
        {ping, B} -> Ping(B);
        {pong, B} -> Pong(B)
    end.

encodeOutFrameImpl(FromFrame) ->
    FromFrame(
        fun (B) -> {text, B} end,
        fun (B) -> {binary, B} end,
        fun (B) -> {ping, B} end,
        fun (B) -> {pong, B} end
    ).
