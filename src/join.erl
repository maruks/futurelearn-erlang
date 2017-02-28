-module(join).

-import(lists,[foldr/3,foldl/3,filter/2,split/2,flatmap/2,map/2,seq/2]).

-export([join/2,concat/1,member/2,qsort/1,msort/1,isort/1,perms/1]).

join_([X|Xs],Ys) ->
    join_(Xs, [X | Ys]);
join_([], Ys) ->
    Ys.

reverse(Xs) ->
    foldl(fun (E,A) -> [E|A] end, [], Xs).

join(Xs, Ys) ->
    join_(reverse(Xs), Ys).

concat(Xs) ->
    foldr(fun join/2, [], Xs).

member(_, []) ->
    false;
member(X, [X|_]) ->
    true;
member(X, [_|Ys]) ->
    member(X,Ys).

qsort([]) ->
    [];
qsort([X|Xs]) ->
    L = filter(fun(E) -> E =< X end, Xs),
    R = filter(fun(E) -> E > X end, Xs),
    qsort(L) ++ [ X | qsort(R)].

merge([X|Xs]=A, [Y|Ys]=B) ->
    if X < Y ->
	    [ X | merge(Xs, B)];
       true ->
	    [ Y | merge(A, Ys)]
    end;
merge(A, []) ->
    A;
merge([], B) ->
    B.

msort([]=E) ->
    E;
msort([_]=E) ->
    E;
msort(L) ->
    {Xs,Ys} = split(length(L) div 2, L),
    merge(msort(Xs), msort(Ys)).

insert(Y, []) ->
    [Y];
insert(Y, [X|Xs]=E) ->
    if Y =< X ->
	    [Y | E];
       true -> [X | insert(Y, Xs)]
    end.

isort([]=E) ->
    E;
isort([X|Xs]) ->
    insert(X, isort(Xs)).

take_nth(N,Xs) ->
    {L,[R|Rs]} = split(N,Xs),
    {R,L ++ Rs}.

perms([]) ->
    [[]];
perms(Xs) ->
    S = map(fun(N) -> take_nth(N,Xs) end, seq(0, length(Xs)-1)),
    flatmap(fun({E,L}) -> map(fun(P) -> [E | P] end, perms(L)) end, S).
