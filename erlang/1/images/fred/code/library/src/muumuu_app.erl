-module(muumuu_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_Type, _Args) ->
    muumuu_sup:start_link().

stop(_) ->
    ok.
