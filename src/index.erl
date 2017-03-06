-module(index).

-import(lists,[zip/2,foldl/3,seq/2,map/2,filter/2,reverse/1]).

-export([index_file/1]).

get_file_contents(Name) ->
    {ok,File} = file:open(Name,[read]),
    Rev = get_all_lines(File,[]),
    lists:reverse(Rev).

get_all_lines(File,Partial) ->
    case io:get_line(File,"") of
        eof -> file:close(File),
               Partial;
        Line -> {Strip,_} = lists:split(length(Line)-1,Line),
                get_all_lines(File,[Strip|Partial])
    end.

%% ------------------------------------------------------------------

index_file(File) ->
    C = get_file_contents(File),
    index(C).

tuples(Ns) ->
    TList = foldl(fun(N, Ts) -> case Ts of
				  [{F, P} | R] when N - P < 2 -> [{F, N} | R];
				  Xs -> [{N, N} | Xs]
			       end
		  end,
		  [], Ns),
    reverse(TList).

index(Lines) ->
    Idx = maps:to_list(index_words(Lines)),
    map(fun({W , Ns}) -> {W, tuples(reverse(Ns))} end, Idx).

update_map(Words, Idx, Map) ->
    foldl(fun(W, M) -> maps:update_with(W, fun(V) -> [Idx|V] end,[Idx],M) end, Map, Words).

index_words(Lines) ->
    Words = map(fun words/1, Lines),
    element(2, foldl(fun(Ws, {Idx, Map}) -> {1 + Idx, update_map(Ws, Idx, Map)} end, {1, maps:new()}, Words)).

words(Line) ->
    Lower = string:to_lower(Line),
    Tokens = string:tokens(Lower," -,.!:;\"\'\\"),
    filter(fun(W) -> length(W) > 2 end, Tokens).
