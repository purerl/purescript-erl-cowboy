-module(erl_cowboy_req@foreign).
-export([ok/0,reply/4,replyWithoutBody/3,replyStatus/2,method/1,versionImpl/4,scheme/1,host/1,port/1,path/1,qs/1,headerImpl/4,headers/1,setHeader/3,setBody/2, setCookie/3, peer/1]).

ok() -> ok.

reply(Status, Headers, Body, Req) -> cowboy_req:reply(Status, Headers, Body, Req).

replyWithoutBody(Status, Headers, Req) -> cowboy_req:reply(Status, Headers, Req).

replyStatus(Status, Req) -> cowboy_req:reply(Status, Req).

method(Req) -> cowboy_req:method(Req).

versionImpl(V10, V11, V20, Req) -> case cowboy_req:version(Req) of
  'HTTP/1.0' -> V10;
  'HTTP/1.1' -> V11;
  'HTTP/2.0' -> V20
end.

scheme(Req) -> cowboy_req:scheme(Req).

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
