-module(palin).
-export([start/1,stop/0,check/1,server/0,distribute/1,dist/2,trace/0,stop_trace/0, client/1,run_client/1]).

% palindrome problem
%
% palindrome("Madam I\'m Adam.") = true

palindrome(Xs) ->
    palin(nocaps(nopunct(Xs))).

nopunct([]) ->
    [];
nopunct([X|Xs]) ->
    case lists:member(X,".,\ ;:\t\n\'\"") of
	true ->
	    nopunct(Xs);
	false ->
	    [ X | nopunct(Xs) ]
    end.

nocaps([]) ->
    [];
nocaps([X|Xs]) ->
    [ nocap(X) | nocaps(Xs) ].

nocap(X) ->
    case $A =< X andalso X =< $Z of
	true ->
	    X+32;
	false ->
	    X
    end.

% literal palindrome

palin(Xs) ->
    Xs == reverse(Xs).

reverse(Xs) ->
    shunt(Xs,[]).

shunt([],Ys) ->
    Ys;
shunt([X|Xs],Ys) ->
    shunt(Xs,[X|Ys]).

% -----------------------------

dist(Servers, N) ->
    receive
	{check, _, _} = M ->
	    lists:nth(N + 1, Servers) ! M,
	    dist(Servers, (N + 1) rem length(Servers));
	stop ->
	    lists:foreach(fun(P) -> P ! stop end, Servers),
	    ok
    end.

distribute(N) ->
    Servers = [ spawn(?MODULE, server,[]) || _ <- lists:seq(1,N)],
    spawn(?MODULE, dist, [Servers, 0]).

server() ->
    receive
	{check, From, Str} ->
	    From ! {result, palindrome(Str)},
	    server();
	stop ->
	    bye
    end.

start(N) ->
    register(server, distribute(N)).

stop() ->
    server ! stop.

check(Str) ->
    server ! {check, self(), Str}.

client([]) ->
    bye;
client([X|Xs]) ->
    check(X),
    receive
	M ->
	    io:format("~p~n",[M]),
	    client(Xs)
    end.

run_client(Strs) ->
    spawn(?MODULE,client,[Strs]).

trace() ->
    dbg:start(),
    dbg:tracer(),
    dbg:p(server, m).

stop_trace() ->
    dbg:stop_clear().
