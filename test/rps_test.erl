-module(rps_test).
-import(rps,[play/1,echo/1,play_two/3,rock/1,no_repeat/1,const/1,enum/1,cycle/1,rand/1,val/1,tournament/2,most_frequent/1,least_frequent/1,strategy_vs_strategy/3,rand_strategy/1,best_strategy/1]).

-include_lib("eunit/include/eunit.hrl").

no_repeat_test() ->
    ?assertEqual(paper, no_repeat([])),
    ?assertEqual(paper, no_repeat([scissors])),
    ?assertEqual(scissors, no_repeat([rock, paper])),
    ?assertEqual(rock, no_repeat([paper])).

most_frequent_test() ->
    ?assertEqual(paper, most_frequent([rock,paper,scissors,rock])),
    ?assertEqual(scissors,most_frequent([rock,paper,scissors,paper,rock,paper])),
    ?assertEqual(rock, most_frequent([scissors])).

least_frequent_test() ->
    ?assertEqual(scissors, least_frequent([rock,scissors,rock])),
    ?assertEqual(rock,least_frequent([rock,paper,scissors,paper,rock,paper])),
    ?assertEqual(paper, least_frequent([scissors,paper])).

strategy_vs_strategy_test() ->
    Strategies = [fun rps:no_repeat/1,
		  fun rps:cycle/1,
		  fun rps:least_frequent/1,
		  fun rps:most_frequent/1,
		  fun rps:rand/1,
		  fun rps:rock/1,
		  fun rps:echo/1],
    ?assert(strategy_vs_strategy(fun rps:no_repeat/1, fun rps:cycle/1, 10) > 0),
    ?assert(strategy_vs_strategy(fun rps:cycle/1, fun rps:echo/1, 10) > 0),
    ?assert(strategy_vs_strategy(fun rps:rock/1, fun rps:no_repeat/1, 10) > 0),
    ?assert(strategy_vs_strategy(fun rps:rock/1, fun rps:least_frequent/1, 10) > 0),
    ?assert(strategy_vs_strategy(fun rps:most_frequent/1, fun rps:rock/1, 10) > 0),
    ?assert(strategy_vs_strategy(best_strategy(Strategies), fun rps:cycle/1 , 200) > 0).
