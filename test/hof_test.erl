-module(hof_test).
-import(hof,[iterate/1,twice/1,compose/1]).

-include_lib("eunit/include/eunit.hrl").

twice_test() ->
    TwoTwice = twice(fun hof:twice/1),
    Mul3 = fun(X) -> X * 3 end,
    F = TwoTwice(Mul3),
    ?assertEqual(162, F(2)).

compose_test() ->
    Inc = fun(X) -> X + 1 end,
    Mul2 = fun(X) -> X * 2 end,
    Add3 = fun(X) -> X + 3 end,
    F = hof:compose([Add3, Mul2, Inc]),
    ?assertEqual(5, F(0)).

iterate_test() ->
    Mul2 = fun(X) -> X * 2 end,
    F = (iterate(4))(Mul2),
    ?assertEqual(32,F(2)).
