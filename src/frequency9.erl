%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency9).
-export([start_server/1,init_server/1,allocate/0,deallocate/1,stop/0,client/1,start_clients/0]).
-export([router_init/0,router_loop/2,start/0]).

-import(maps,[get/2,keys/1,from_list/1,update/3]).

%% These are the start functions used to create and
%% initialize the server.

new_table() ->
    ets:new(state, [set, public, named_table]).

dump_state(State) ->
    ets:insert(state, {self(),State}).

get_state(Pid) ->
    [{_, State}] = ets:lookup(state, Pid),
    State.

router_init() ->
    process_flag(trap_exit, true),
    new_table(),
    Freqs1 = lists:seq(10,15),
    Freqs2 = lists:seq(20,25),
    Server1 = start_server({Freqs1,[]}),
    Server2 = start_server({Freqs2,[]}),
    Servers = from_list([{hd(Freqs1), Server1}, {hd(Freqs2), Server2}]),
    router_loop(Servers, 0).

router_loop(Servers, Allocations) ->
    receive
	{'EXIT', Pid, Reason} ->
	    io:format("server exit ~p ~p~n",[Pid,Reason]),
	    State = get_state(Pid),
	    Freq = min_freq(State),
	    NewPid = start_server(State),
	    router_loop(update(Freq, NewPid, Servers), Allocations);
	{request, _Pid, allocate} = M ->
	    Index = 1 + Allocations rem 2,
	    Server = get(lists:nth(Index, keys(Servers)), Servers),
	    Server ! M,
	    router_loop(Servers, Allocations + 1);
	 {request, _Pid , {deallocate, Freq}} = M ->
	    Server = get(first_freq(Freq), Servers),
	    Server ! M,
	    router_loop(Servers, Allocations);
	{request, Pid, stop} ->
	    Pid ! {reply, stopped}
    end.

first_freq(N) when N>15 ->
    20;
first_freq(_) ->
    10.

min_freq({Free, Allocated}) ->
    lists:foldl(fun erlang:min/2, 99, Free ++ lists:map(fun({A,_}) -> A end, Allocated)).

start() ->
    register(?MODULE, spawn(?MODULE,router_init,[])).

start_server(Freqs) ->
    spawn_link(?MODULE, init_server, [Freqs]).

init_server(Freqs) ->
  process_flag(trap_exit, true),
  dump_state(Freqs),
  loop(Freqs).

%% The Main Loop

loop(Frequencies) ->
    receive
	{request, Pid, allocate} ->
	    {NewFrequencies, Reply} = allocate(Frequencies, Pid),
	    Pid ! {reply, Reply},
	    dump_state(NewFrequencies),
	    loop(NewFrequencies);
	{request, Pid , {deallocate, Freq}} ->
	    try deallocate(Frequencies, Freq) of
		NewFrequencies ->
		    Pid ! {reply, {ok}},
		    dump_state(NewFrequencies),
		    loop(NewFrequencies)
	    catch error:{badmatch,false} ->
		    Pid ! {reply, {error, unallocated_frequency}},
		    loop(Frequencies)
	    end;
 	{'EXIT', Pid, _Reason} ->
	    NewFrequencies = exited(Frequencies, Pid),
	    dump_state(NewFrequencies),
	    loop(NewFrequencies)
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),
  unlink(Pid),
  NewAllocated = lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,Pid}} ->
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        {[Freq|Free],NewAllocated};
      false ->
        {Free,Allocated}
    end.

%% Functional interface

allocate() ->
    ?MODULE ! {request, self(), allocate},
    receive
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) ->
    ?MODULE ! {request, self(), {deallocate, Freq}},
    receive
	    {reply, Reply} -> Reply
    end.

stop() ->
    ?MODULE ! {request, self(), stop},
    receive
	    {reply, Reply} -> Reply
    end.

client(Sleep) ->
    process_flag(trap_exit, true),
    client_loop(Sleep, none, false).

client_action(true, Freq) ->
    deallocate(Freq);
client_action(false, _) ->
    allocate().

client_loop(Sleep, Freq, Allocated) ->
    timer:sleep(Sleep),
    case client_action(Allocated, Freq) of
	{ok, F} ->
	    io:format("allocated ~p~n",[F]),
	    client_loop(Sleep, F, true);
	{ok} ->
	    io:format("deallocated~n"),
	    client_loop(Sleep, none, false);
	{error, Reason} ->
	    io:format("error ~p~n",[Reason]),
	    client_loop(Sleep, none, false)
    end.

start_clients() ->
    [spawn(?MODULE,client,[S*500]) || S <- lists:seq(1,6)].
