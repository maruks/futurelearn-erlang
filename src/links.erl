-module(links).
-export([start/0,loop/1,init/1]).

action(1) ->
    spawn_link(fun() -> ok end);
action(2) ->
    spawn_link(fun() -> exit(reason) end);
action(3) ->
    spawn_link(fun() -> exit(normal) end);
action(4) ->
    spawn_link(fun() -> 1/0 end);
action(5) ->
    spawn_link(fun() -> erlang:error(reason) end);
action(6) ->
    spawn_link(fun() -> throw(rocks) end);
action(7) ->
    exit(self(), normal);   %  exit/2
action(8) ->
    exit(spawn_link(fun() -> timer:sleep(50000) end), normal);
action(9) ->
    exit(spawn_link(fun() -> timer:sleep(50000) end), reason);
action(10) ->
    exit(spawn_link(fun() -> timer:sleep(50000) end), kill);
action(11) ->
    spawn_link(fun() -> exit(kill) end);
action(12) ->
    exit(self(), kill).

init(Xs) ->
    process_flag(trap_exit,true),
    loop(Xs).

loop([]) ->
    bye;
loop([X|Xs]) ->
    io:format("~p~n",[X]),
    action(X),
    receive
	M ->
	    io:format("~p~n",[M]),
	    timer:sleep(2000),
	    loop(Xs)
    after 1000 ->
	    loop(Xs)
    end.

start() ->
    spawn(?MODULE,init,[lists:seq(1,12)]).
