-module(dining).

-import(lists,[foldr/3,foldl/3,filter/2,split/2,flatmap/2,map/2,seq/2]).

-export([run/1,fork_loop/2,philosopher_loop/5,start_philosopher/3,report_loop/1]).

fork_loop(Available, Owner) ->
    receive
	{take, From} when Available == true ->
	    From ! {taken, self()},
	    fork_loop(false, From);
	{take, From} when Available == false ->
	    From ! {not_available},
	    fork_loop(false, Owner);
	{return, Owner} when Available == false ->
	    Owner ! {returned, self()},
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
    timer:sleep(1000),
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
	    100 ->
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


report_loop(Eating) ->
    receive
	{eating, _} ->
	    report_loop(Eating + 1);
	{thinking, _} ->
	    report_loop(Eating - 1);
	{print} ->
	    io:format("~p~n",[Eating]),
	    report_loop(Eating)
    end.

spawn_philosophers(0, PrevFork, FirstFork) ->
    spawn(?MODULE, start_philosopher, [0, PrevFork, FirstFork]);
spawn_philosophers(N, PrevFork, FirstFork) ->
    RightFork = spawn(?MODULE, fork_loop, [true, none]),
    spawn(?MODULE, start_philosopher, [N, PrevFork, RightFork]),
    spawn_philosophers(N - 1, RightFork, FirstFork).

spawn_philosophers(N) ->
    FirstFork = spawn(?MODULE, fork_loop, [true, none]),
    spawn_philosophers(N - 1, FirstFork, FirstFork).

run(Number) ->
    Reporter = spawn(?MODULE, report_loop, [0]),
    register(reporter, Reporter),
    timer:send_interval(1000, reporter, {print}),
    spawn_philosophers(Number).
