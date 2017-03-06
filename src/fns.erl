-module(fns).

-import(lists,[foldl/3,map/2,filter/2]).

-export([doubleAll/1,evens/1,product/1,zip/2,zip_with/3]).


doubleAll(Xs) ->
    map(fun(E) -> E * 2 end, Xs).

evens(Xs) ->
    filter(fun(E) -> E rem 2 == 0 end, Xs).

product(Xs) ->
    foldl(fun erlang:'*'/2, 1 , Xs).

%%

zip_with(Fn, [X|Xs],[Y|Ys]) ->
    [Fn(X,Y) | zip_with(Fn, Xs, Ys)];
zip_with(_, _, _) ->
    [].

zip(Xs,Ys) ->
    zip_with(fun(A,B) -> {A,B} end, Xs, Ys).
