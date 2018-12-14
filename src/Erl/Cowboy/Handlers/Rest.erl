-module(erl_cowboy_handlers_rest@foreign).
-export([initResult/2, restResult/3, stop/2, switchHandler/3, contentTypesAcceptedResult/1, contentTypesProvidedResult/1, authorized/0, unauthorized/1, notMoved/0, moved/1, optionsResponse/0]).

initResult(S, Req) -> {cowboy_rest, Req, S}.

restResult(R, S, Req) -> {R, Req, S}.

stop(S, Req) -> {stop, Req, S}.

switchHandler(Module, State, Req) -> {{switch_handler, Module}, Req, State}.

convertContentType({{simpleContentType, ContentType}, CB}) ->
  {ContentType, CB};

convertContentType({{contentType, T, ST, Params}, CB}) ->
    Params1 = case Params of
        {anyParams} -> '*';
        {contentTypeParams, ActualParams} -> ActualParams
    end,
    {{T, ST, Params1}, CB}.

contentTypesAcceptedResult(List) -> lists:map(fun convertContentType/1, List).
contentTypesProvidedResult(List) -> lists:map(fun convertContentType/1, List).

authorized() -> true.
unauthorized(Header) -> {false, Header}.

notMoved() -> false.
moved(Uri) -> {true, Uri}.

optionsResponse() -> ok.
