-module(rps).
-define(ALL_MOVES,[rock, paper, scissors]).
-import(lists,[filter/2,any/2,all/2,nth/2,foldl/3,sort/2,map/2,reverse/1,sub_list/2,seq/2]).
-import(maps,[update_with/4,new/0,to_list/1]).
-export([play/1,echo/1,play_two/3,rock/1,no_repeat/1,const/1,enum/1,cycle/1,rand/1,val/1,tournament/2,most_frequent/1,least_frequent/1,rand_strategy/1,best_strategy/1,strategy_vs_strategy/3]).

%
% play one strategy against another, for N moves.
%

play_two(StrategyL, StrategyR, N) ->
    play_two(StrategyL, StrategyR, [], [], N).

play_two(_, _, PlaysL, PlaysR, 0) ->
    tournament(PlaysL, PlaysR);
play_two(StrategyL, StrategyR, PlaysL, PlaysR, N) ->
    MoveL = StrategyL(PlaysR),
    MoveR = StrategyR(PlaysL),
    play_two(StrategyL, StrategyR, [MoveL | PlaysL], [MoveR | PlaysR], N - 1).

%
% interactively play against a strategy, provided as argument.
%

play(Strategy) ->
    io:format("Rock - paper - scissors~n"),
    io:format("Play one of rock, paper, scissors, ...~n"),
    io:format("... r, p, s, stop, followed by '.'~n"),
    play(Strategy,[],[]).

% tail recursive loop for play/1

play(Strategy, MyMoves, Moves) ->
    {ok,P} = io:read("Play: "),
    Play = expand(P),
    case Play of
	stop ->
	    Result = tournament(MyMoves,Moves),
	    io:format("Overall result ~p~n",[Result]);
	_    ->
	    Move = Strategy(MyMoves),
	    Result = result(Play,Move),
	    io:format("Result: ~p~n",[Result]),
	    play(Strategy,[Play| MyMoves], [Move | Moves])
    end.

%
% auxiliary functions
%

% transform shorthand atoms to expanded form

expand(r) -> rock;
expand(p) -> paper;
expand(s) -> scissors;
expand(X) -> X.

% result of one set of plays

result(rock,rock) -> draw;
result(rock,paper) -> lose;
result(rock,scissors) -> win;
result(paper,rock) -> win;
result(paper,paper) -> draw;
result(paper,scissors) -> lose;
result(scissors,rock) -> lose;
result(scissors,paper) -> win;
result(scissors,scissors) -> draw.

% result of a tournament

tournament(PlaysL,PlaysR) ->
    lists:sum(
      lists:map(fun outcome/1,
		lists:zipwith(fun result/2, PlaysL, PlaysR))).

outcome(win)  ->  1;
outcome(lose) -> -1;
outcome(draw) ->  0.

% transform 0, 1, 2 to rock, paper, scissors and vice versa.

enum(0) ->
    rock;
enum(1) ->
    paper;
enum(2) ->
    scissors.

val(rock) ->
    0;
val(paper) ->
    1;
val(scissors) ->
    2.

% give the play which the argument beats.

beats(rock) ->
    scissors;
beats(paper) ->
    rock;
beats(scissors) ->
    paper.

frequencies(Xs) ->
    Result = foldl(fun(Elem, Map) ->
			   update_with(Elem,
				       fun(F)-> F + 1
				       end, 1, Map)
		   end,new(), Xs),
    to_list(Result).

%
% strategies.
%
echo([]) ->
     paper;
echo([Last | _]) ->
    Last.

rock(_) ->
    rock.

no_repeat([]) ->
    paper;
no_repeat([X|_]) ->
    PossibleMoves = ?ALL_MOVES -- [X],
    hd(filter(fun(M) ->
		      all(fun(P) -> M =/= beats(P)
			  end, PossibleMoves)
	      end, ?ALL_MOVES)).

const(Play) ->
    fun(_) -> Play end.

cycle(Xs) ->
    nth(1 + length(Xs) rem 3, ?ALL_MOVES).

rand(_) ->
    nth(rand:uniform(3), ?ALL_MOVES).

most_frequent([]) ->
    paper;
most_frequent(Xs) ->
    Fs = frequencies(Xs),
    [{E, _} | _] = sort(fun({_, A}, {_, B}) -> A >= B end, Fs),
    hd(filter(fun(X)-> E == beats(X) end, ?ALL_MOVES)).

least_frequent([]) ->
    paper;
least_frequent(Xs) ->
    Fs = frequencies(?ALL_MOVES ++ Xs),
    [{E, _} | _] = sort(fun({_, A}, {_, B}) -> A =< B end, Fs),
    hd(filter(fun(X)-> E == beats(X) end, ?ALL_MOVES)).

rand_strategy(Strategies) ->
    fun(Xs) ->
	    S = nth(rand:uniform(length(Strategies)), Strategies),
	    S(Xs)
    end.

play_strategy(_S, PlaysL, PlaysR, []) ->
    tournament(PlaysL, PlaysR);
play_strategy(S, PlaysL, PlaysR, [X|Xs]) ->
    play_strategy(S, [S(PlaysR) | PlaysL], [X|PlaysR], Xs).

best_strategy(Strategies) ->
    fun([]) ->
	    (hd(Strategies))([]);
       (Xs) ->
	    Results = [ {S, play_strategy(S,[],[],reverse(Xs))} || S <- Strategies],
	    [{E, _} | _] = sort(fun({_, A}, {_, B}) -> A >= B end, Results),
	    E(Xs)
    end.

strategy_vs_strategy_(Plays1, Plays2, _, _, 0) ->
    tournament(Plays1, Plays2);
strategy_vs_strategy_(Plays1, Plays2, S1, S2, N) ->
    strategy_vs_strategy_([S1(Plays2) | Plays1], [S2(Plays1) | Plays2], S1, S2, N - 1).

strategy_vs_strategy(S1, S2, N) ->
    strategy_vs_strategy_([], [], S1, S2, N).
