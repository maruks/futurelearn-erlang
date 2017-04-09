-module(dining).

-define(EAT_TIME, 1000).
-define(HUNGRY_AFTER, 100).

-import(lists,[foldr/3,foldl/3,filter/2,split/2,flatmap/2,map/2,seq/2]).

-export([start/1,stop/0,fork_loop/2,philosopher_loop/5,start_philosopher/3,reporter/1,main/1]).

fork_loop(Available, Owner) ->
    receive
	{take, From} when Available == true ->
	    From ! {taken, self()},
	    reporter ! {taken, self()},
	    fork_loop(false, From);
	{take, From} when Available == false ->
	    From ! {not_available},
	    fork_loop(false, Owner);
	{return, Owner} when Available == false ->
	    Owner ! {returned, self()},
	    reporter ! {returned, self()},
	    fork_loop(true, none);
	{return, From} ->
	    From ! {error},
	    fork_loop(Available, Owner);
	Msg ->
    	    io:format("error ~p~n", [Msg])
    end.

stop_eating(LeftFork, RightFork, false, false) ->
    philosopher_loop(false, LeftFork, RightFork, false, false);
stop_eating(LeftFork, RightFork, HasLeftFork, HasRightFork) ->
    receive
	{returned, LeftFork} ->
	    stop_eating(LeftFork, RightFork, false, HasRightFork);
	{returned, RightFork} ->
	    stop_eating(LeftFork, RightFork, HasLeftFork, false)
    end.

philosopher_loop(true, LeftFork, RightFork, true, true) ->
    reporter ! {eating, self()},
    timer:sleep(?EAT_TIME),
    reporter ! {thinking, self()},
    RightFork ! {return, self()},
    LeftFork ! {return, self()},
    stop_eating(LeftFork, RightFork, true, true);
philosopher_loop(false, LeftFork, RightFork, HasLeftFork, HasRightFork) ->
    receive
	{taken, LeftFork} ->
	    philosopher_loop(HasRightFork, LeftFork, RightFork, true, HasRightFork);
	{taken, RightFork} ->
	    philosopher_loop(HasLeftFork, LeftFork, RightFork, HasLeftFork, true);
	_ ->
	    philosopher_loop(false, LeftFork, RightFork, HasLeftFork, HasRightFork)
	after
	    ?HUNGRY_AFTER ->
		LeftFirst = rand:uniform(10) < 5 ,
		case LeftFirst of
		      true -> LeftFork ! {take, self()};
		      false -> RightFork ! {take, self()}
		  end,
		  philosopher_loop(false, LeftFork, RightFork, HasLeftFork, HasRightFork)
    end.

start_philosopher(N, LeftFork, RightFork) ->
    put(num, N),
    philosopher_loop(false, LeftFork, RightFork, false, false).

-record(report, {eating = 0, consumed = 0, forks = 0}).

report_loop(#report{eating= Eating, consumed = Consumed, forks = Forks} = S) ->
    receive
	{eating, _} ->
	    report_loop(S#report{eating= Eating + 1, consumed = Consumed + 1});
	{thinking, _} ->
	    report_loop(S#report{eating= Eating - 1});
	{taken, _} ->
	    report_loop(S#report{forks= Forks - 1});
	{returned, _} ->
	    report_loop(S#report{forks= Forks + 1});
	{print} ->
	    io:format("eating = ~p , forks = ~p, consumed = ~p~n",[Eating, Forks, Consumed]),
	    report_loop(S);
	{stop} ->
	    exit("bye")
    end.

spawn_philosophers(0, PrevFork, FirstFork) ->
    spawn_link(?MODULE, start_philosopher, [0, PrevFork, FirstFork]);
spawn_philosophers(N, PrevFork, FirstFork) ->
    RightFork = spawn_link(?MODULE, fork_loop, [true, none]),
    spawn_link(?MODULE, start_philosopher, [N, PrevFork, RightFork]),
    spawn_philosophers(N - 1, RightFork, FirstFork).

spawn_philosophers(N) ->
    FirstFork = spawn_link(?MODULE, fork_loop, [true, none]),
    spawn_philosophers(N - 1, FirstFork, FirstFork).

reporter(Number) ->
    spawn_philosophers(Number),
    report_loop(#report{forks = Number}).

start(Number) ->
    Reporter = spawn(?MODULE, reporter, [Number]),
    register(reporter, Reporter),
    timer:send_interval(1000, reporter, {print}).

stop() ->
    reporter ! {stop}.

main([N]) ->
    start(list_to_integer(atom_to_list(N))).
