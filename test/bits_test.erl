-module(bits_test).
-import(bits,[bits/1, bits2/1, bits3/1]).

-include_lib("eunit/include/eunit.hrl").

bits_test() ->
    ?assertEqual(1,bits(8)),
    ?assertEqual(1,bits2(8)),
    ?assertEqual(1,bits3(8)),
    ?assertEqual(9,bits(2#010110101011101)),
    ?assertEqual(9,bits2(2#010110101011101)),
    ?assertEqual(9,bits3(2#010110101011101)).
