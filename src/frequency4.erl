%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency4).
-export([start/0,allocate/0,deallocate/1,stop/0,client/1,start_clients/0]).
-export([supervisor_init/0,supervisor_loop/0,supervisor_start/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

supervisor_init() ->
    process_flag(trap_exit, true),
    supervisor_loop().

supervisor_loop() ->
    start(),
    receive
	{'EXIT', Pid, Reason} ->
	    io:format("EXIT ~p ~p~n",[Pid,Reason]),
	    supervisor_loop()
    end.

supervisor_start() ->
    spawn(?MODULE,supervisor_init,[]).

start() ->
    register(frequency,
	     spawn_link(frequency, init, [])).

init() ->
  process_flag(trap_exit, true),    %%% ADDED
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      Pid ! {reply, Reply},
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      NewFrequencies = deallocate(Frequencies, Freq),
      Pid ! {reply, ok},
      loop(NewFrequencies);
    {request, Pid, stop} ->
      Pid ! {reply, stopped};
    {'EXIT', Pid, _Reason} ->                   %%% CLAUSE ADDED
      NewFrequencies = exited(Frequencies, Pid),
      loop(NewFrequencies)
  end.

%% Functional interface

allocate() ->
    frequency ! {request, self(), allocate},
    receive
	    {reply, Reply} -> Reply
    end.

deallocate(Freq) ->
    frequency ! {request, self(), {deallocate, Freq}},
    receive
	    {reply, Reply} -> Reply
    end.

stop() ->
    frequency ! {request, self(), stop},
    receive
	    {reply, Reply} -> Reply
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  link(Pid),                                               %%% ADDED
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),  %%% ADDED
  unlink(Pid),                                             %%% ADDED
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

exited({Free, Allocated}, Pid) ->                %%% FUNCTION ADDED
    case lists:keysearch(Pid,2,Allocated) of
      {value,{Freq,Pid}} ->
        NewAllocated = lists:keydelete(Freq,1,Allocated),
        {[Freq|Free],NewAllocated};
      false ->
        {Free,Allocated}
    end.

%%%

client(Sleep) ->
    process_flag(trap_exit, true),
    client_loop(Sleep, none, false).

client_action(true, Freq) ->
    deallocate(Freq);
client_action(false, _) ->
    allocate().

client_loop(Sleep, Freq, Allocated) ->
    timer:sleep(Sleep),
    try client_action(Allocated, Freq) of
	{ok, F} ->
	    io:format("allocated ~p~n",[F]),
	    client_loop(Sleep, F, true);
	{ok} ->
	    io:format("deallocated~n"),
	    client_loop(Sleep, none, false);
	{error, Reason} ->
	    io:format("error ~p~n",[Reason]),
	    client_loop(Sleep, none, false)
    catch T:E ->
	    io:format("~p~p~n",[T,E]),
	    client_loop(Sleep, Freq, Allocated)
    end.

start_clients() ->
    [spawn(?MODULE,client,[S]) || S <- [1000,2000,3000,4000]].
