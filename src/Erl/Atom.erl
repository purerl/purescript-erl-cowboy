-module(erl_atom@foreign).
-export([atom/1]).

% TODO: Fails on higher unicode chars! Currently.
atom(S) -> binary_to_atom(S, utf8).
