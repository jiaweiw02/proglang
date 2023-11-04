-module(combinations).
-export([start/2, comb/4, factnk/2]).

factnk(_, 0) -> 1;
factnk(N, K) -> N * factnk(N-1, K-1).

comb(N, K, Case, Customer) -> Customer ! {Case, factnk(N, K)}.

start(N, K) ->
    spawn(combinations, comb, [N, K, num, self()]),
    spawn(combinations, comb, [K, K, den, self()]),

    receive {num, Num} -> receive {den, Den} -> Num / Den end end.

% c(combinations), combinations:start(10,2).