%%%-------------------------------------------------------------------
%% @doc cowboy_server public API
%% @end
%%%-------------------------------------------------------------------
-module(cowboy_server_app).
-behaviour(application).
-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    cowboy_server_sup:start_link().

stop(_State) ->
    ok.