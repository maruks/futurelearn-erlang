-module(join_tests).
-import(join,[join/2,concat/1,member/2,qsort/1,msort/1,isort/1,perms/1]).

-include_lib("eqc/include/eqc.hrl").
-include_lib("eunit/include/eunit.hrl").

-compile(export_all).

prop_join() ->
    ?FORALL(A, list(int()), ?FORALL(B, list(int()), join(A,B) == A++B)).

prop_concat() ->
    ?FORALL(A, list(list(int())), concat(A) == lists:concat(A)).

small_int() ->
    ?SUCHTHAT(N, nat(), N < 100).

prop_member() ->
    ?FORALL(A, small_int(), ?FORALL(B, list(small_int()), member(A,B) == lists:member(A,B))).

prop_qsort() ->
    ?FORALL(A, list(int()), qsort(A) == lists:sort(A)).

prop_isort() ->
    ?FORALL(A, list(int()), isort(A) == lists:sort(A)).

prop_msort() ->
    ?FORALL(A, list(int()), msort(A) == lists:sort(A)).

fact(0) ->
    1;
fact(N) ->
    N * fact(N-1).

small_list() ->
    ?SUCHTHAT(N, ?LET(Values, list(int()), lists:usort(Values)), length(N) < 10).

prop_perms() ->
    ?FORALL(A, small_list(),
	    length(perms(A)) == fact(length(A)) andalso
	    length(lists:usort(perms(A))) == fact(length(A)) andalso
	    lists:all(fun(P) -> length(P) == length(A) end, perms(A))).
