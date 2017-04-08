%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([init/0,start/0,stop/0,alloc/0,dealloc/1]).

%% These are the start functions used to create and
%% initialize the server.

init() ->
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
	    {NewFrequencies, Reply} = deallocate(Frequencies, Pid, Freq),
	    Pid ! {reply, Reply},
	    loop(NewFrequencies);
	{request, Pid, stop} ->
	    Pid ! {reply, stopped}
    end.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({_, Allocated}=F, Pid) ->
    allocate(F, Pid, not lists:keymember(Pid, 2, Allocated)).

allocate(F, _Pid, false) ->
    {F, {error, already_allocated}};
allocate({[], Allocated}, _Pid, true) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid, true) ->
    {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate(F, _, false) ->
    {F, {error, not_allocated}};
deallocate({Free, Allocated}, Freq, true) ->
    NewAllocated=lists:keydelete(Freq, 1, Allocated),
    {{[Freq|Free],  NewAllocated}, {ok}};
deallocate({_, Allocated}=F, Pid, Freq) ->
    deallocate(F, Freq, lists:member({Freq, Pid}, Allocated)).

% ----

start() ->
    Pid = spawn(?MODULE, init, []),
    register(?MODULE, Pid).

alloc() ->
    ?MODULE ! {request, self(), allocate}.

dealloc(Freq) ->
    ?MODULE ! {request, self(), {deallocate, Freq}}.

stop() ->
    ?MODULE ! {request, self(), stop}.
