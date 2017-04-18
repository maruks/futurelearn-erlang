%%%-------------------------------------------------------------------
%% @doc fl app
%% @end
%%%-------------------------------------------------------------------

-module(fl_app).

-behaviour(application).

-export([start/2,stop/1,start/0]).

start() ->
    application:ensure_all_started(fl).

start(_StartType, _StartArgs) ->
    fl_sup:start_link().

stop(_State) ->
    ok.
