-module(paper).

-import(lists,[foldl/3,zipwith/3,sum/1]).

-export([tournament/2]).

beat(rock) ->
    paper;
beat(paper) ->
    scissors;
beat(scissors) ->
    rock.

lose(rock) ->
    scissors;
lose(scissors) ->
    paper;
lose(paper) ->
    rock.

result(Left, Right) ->
    Beat = beat(Left) == Right,
    Lose = lose(Left) == Right,
    if Beat -> -1;
       Lose -> 1;
       true -> 0
    end.

tournament(Xs,Ys) ->
    sum(zipwith(fun result/2, Xs, Ys)).
