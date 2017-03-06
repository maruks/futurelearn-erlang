-module(index_test).

-import(index,[index_file/1]).
-import(lists,[keyfind/3]).

-include_lib("eunit/include/eunit.hrl").

index_test() ->
    Index = index_file("./test/gettysburg-address.txt"),
    ?assertEqual(element(2, keyfind("dead", 1, Index)), [{15,15},{22,22},{25,25}]),
    ?assertEqual(element(2, keyfind("this", 1, Index)), [{2,2},{11,11},{14,14},{26,26}]),
    ?assertEqual(element(2, keyfind("that", 1, Index)), [{3,3},{6,11},{22,27}]),
    ?assertEqual(element(2, keyfind("people", 1, Index)), [{27,28}]),
    ?assertEqual(element(2, keyfind("earth", 1, Index)), [{28,28}]).
