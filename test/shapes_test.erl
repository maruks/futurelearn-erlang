-module(shapes_test).

-import(shapes,[perimeter/1,area/1,enclose/1]).

-include_lib("eunit/include/eunit.hrl").
-include("shapes.hrl").

enclose_test() ->
    ?assertEqual(#rectangle{x=0,y=0,h=2,w=2}, enclose(#circle{x=1,y=1,r=1})).

area_test() ->
    ?assertEqual(15,area(#rectangle{w=3, h=5})).

perimeter_test() ->
    ?assertEqual(16,perimeter(#rectangle{w=3, h=5})).
