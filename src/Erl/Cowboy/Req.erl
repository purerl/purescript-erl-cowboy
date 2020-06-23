-module(erl_cowboy_req@foreign).
-include_lib("kernel/include/file.hrl").
-export([reply/4,replyWithoutBody/3,replyWithFile/4,replyStatus/2,method/1,versionImpl/4,scheme/1,bindingWithDefault/3,bindingImpl/4,pathInfo/1,host/1,port/1,path/1,qs/1,headerImpl/4,headers/1,setHeader/3,setBody/2, setCookie/3, peer/1, readBodyImpl/3, streamReply/3, streamBody/2, streamBodyFinal/2]).

reply(Status, Headers, Body, Req) -> fun () -> cowboy_req:reply(Status, Headers, Body, Req) end.

replyWithFile(Status, Headers, Filename, Req) -> fun () ->
  {ok, #file_info{size = Size}} = file:read_file_info(Filename),
  cowboy_req:reply(Status, Headers, {sendfile, 0, Size, Filename}, Req)
  end.

replyWithoutBody(Status, Headers, Req) -> fun () -> cowboy_req:reply(Status, Headers, Req) end.

replyStatus(Status, Req) -> fun () -> cowboy_req:reply(Status, Req) end.

method(Req) -> cowboy_req:method(Req).

versionImpl(V10, V11, V20, Req) -> case cowboy_req:version(Req) of
  'HTTP/1.0' -> V10;
  'HTTP/1.1' -> V11;
  'HTTP/2' -> V20
end.

scheme(Req) -> cowboy_req:scheme(Req).

bindingWithDefault(Name, Req, Default) ->
  cowboy_req:binding(Name, Req, Default).

bindingImpl(Nothing, Just, Name, Req) ->
  case cowboy_req:binding(Name,  Req) of
    undefined -> Nothing;
    Val -> Just(Val)
  end.

pathInfo(Req) -> cowboy_req:path_info(Req).

host(Req) -> cowboy_req:host(Req).

port(Req) -> cowboy_req:port(Req).

path(Req) -> cowboy_req:path(Req).

qs(Req) -> cowboy_req:qs(Req).

headerImpl(Nothing, Just, H, Req) -> case cowboy_req:header(H, Req) of
  undefined -> Nothing;
  Val -> Just(Val)
end.

headers(Req) -> cowboy_req:headers(Req).

setHeader(Name, Value, Req) -> cowboy_req:set_resp_header(Name, Value, Req).

setCookie(Name, Value, Req) -> cowboy_req:set_resp_cookie(Name, Value, Req).

setBody(Body, Req) -> cowboy_req:set_resp_body(Body, Req).

peer(Req) -> cowboy_req:peer(Req).

readBodyImpl(FullData, PartialData, Req) ->
  fun() -> case cowboy_req:read_body(Req) of
      {ok, D, Req2} -> (FullData(D))(Req2);
      {more, D, Req2} -> (PartialData(D))(Req2)
    end
  end.

streamReply(Status, Headers, Req) -> fun () ->
  cowboy_req:stream_reply(Status, Headers, Req)
end.

streamBody(Data, Req) -> fun () -> cowboy_req:stream_body(Data, nofin, Req) end.
streamBodyFinal(Data, Req) -> fun () -> cowboy_req:stream_body(Data, fin, Req) end.
