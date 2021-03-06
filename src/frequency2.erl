%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency2).
-export([start/0,allocate/0,deallocate/1,stop/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(?MODULE,
	     spawn(?MODULE, init, [])).

init() ->
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% The Main Loop

loop(Frequencies) ->
  case rand:uniform(2) of  % random delay
      1 -> timer:sleep(3000);
      _ -> ok
  end,
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
      Pid ! {reply, stopped}
  end.

%% Functional interface

clear() ->
  receive
      _Msg ->
	  io:format("clear~n",[]),
	  clear()
  after 0 -> ok
  end.

allocate() ->
    clear(),
    ?MODULE ! {request, self(), allocate},
    receive
	    {reply, Reply} -> Reply
    after 50 -> {error, timeout}
    end.

deallocate(Freq) ->
    clear(),
    ?MODULE ! {request, self(), {deallocate, Freq}},
    receive
	{reply, Reply} -> Reply
    after 50 -> {error, timeout}
    end.

stop() ->
    clear(),
    ?MODULE ! {request, self(), stop},
    receive
	{reply, Reply} -> Reply
    after 50 -> {error, timeout}
    end.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.
