-module(bits).

-export([bits/1, bits2/1, bits3/1]).

bits_tail_rec(<<B:1, Rest/bitstring>>, A) ->
    bits_tail_rec(Rest,B + A);
bits_tail_rec(<<>>, A) ->
    A.

bits(N) ->
    bits_tail_rec(binary:encode_unsigned(N), 0).

bits_rec(<<B:1, Rest/bitstring>>) ->
    B + bits_rec(Rest);
bits_rec(<<>>) ->
    0.

bits2(N) ->
    bits_rec(binary:encode_unsigned(N)).

bits3(0) ->
    0;
bits3(N) ->
    1 + bits3(N band (N - 1)).  % turns off rightmost bit
