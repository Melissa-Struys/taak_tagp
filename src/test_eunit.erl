-module(test_eunit).
-export([fac/1]).
%-define(NOTEST, 1).
-include_lib("eunit/include/eunit.hrl").

fac(1) -> 1;
fac(N) when N > 1 -> N * fac(N-1).

fac_test_() ->
    [?_assert(fac(1) =:= 1),
    ?_assert(fac(2) =:= 2),
    ?_assert(fac(3) =:= 6),
    ?_assert(fac(4) =:= 24),
    ?_assert(fac(5) =:= 120),
    ?_assert(fac(10) =:= 3628800),
    ?_assertException(error, function_clause, fac(0))
    ].
