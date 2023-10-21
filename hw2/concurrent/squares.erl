-module(squares).
-export([sumsquares/1,square/0]).

sumsquares(N) ->
    Workers = create_actors(N),
    send_squares(Workers,N,self()),
    Sum = receive_results(N),
    io:format("The sum of first ~w squares is: ~w~n",[N,Sum]).

create_actors(0) -> [];
create_actors(N) ->
    [spawn(squares,square,[])|create_actors(N-1)].

send_squares([],_,_) -> true;
send_squares([Worker|Workers], N, Farmer) ->
    Worker!{Farmer,N}, 
    send_squares(Workers,N-1, Farmer).

receive_results(N) -> 
    receive_results_aux(N,0).

receive_results_aux(0,Acc) -> Acc;
receive_results_aux(N,Acc) ->
    receive
	S -> receive_results_aux(N-1,Acc+S)
    end.

square() -> 
    receive 
	    {Farmer,N} -> Farmer!N*N
    end.