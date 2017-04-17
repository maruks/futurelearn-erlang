%% Based on code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency_gs).
-behaviour(gen_server).

% an implementation of this is included.
-export([start_link/0]).

% you need to implement these functions.
-export([init/1, handle_call/3, handle_cast/2]).

% these are implemented for you.
-export([handle_info/2, terminate/2, code_change/3]).

% you will need to implement these.
-export([allocate/0,deallocate/1,stop/0,report/0,inject/1]).

%% These are the start functions used to create and
%% initialize the server.

start_link() ->
    gen_server:start_link(
		{local, ?MODULE},
		?MODULE, [], []).

init([]) ->
  Frequencies = {get_frequencies(), []},
  {ok, Frequencies}.

% Hard Coded
get_frequencies() -> [10,11,12,13,14,15].

%% Functional interface

allocate() ->
    gen_server:call(?MODULE, allocate).

deallocate(Freq) ->
    gen_server:cast(?MODULE, {deallocate, Freq}).

stop() ->
    gen_server:stop(?MODULE).

report() ->
    gen_server:call(?MODULE, report).

inject(Freqs) when is_list(Freqs) ->
    gen_server:call(?MODULE, {inject, Freqs}).

% handle callbacks

handle_call(allocate, From, State) ->
    {NewState, Reply} = allocate(State, From),
    {reply, Reply, NewState};
handle_call(report, _From, {F,A} = State) ->
    {reply, {length(F),length(A)}, State};
handle_call({inject, Freqs}, _From, State) ->
    Uniq = lists:usort(Freqs),
    case length(Uniq) == length(Freqs) of
	true ->
	    {reply, {ok}, {Uniq, []}};
	false ->
	    {reply, {error, duplicate}, State}
    end.

handle_cast({deallocate, Freq}, State) ->
  {noreply, deallocate(State, Freq)}.

%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free, Allocated}, Freq) ->
  NewAllocated=lists:keydelete(Freq, 1, Allocated),
  {[Freq|Free],  NewAllocated}.

% default implementations

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
