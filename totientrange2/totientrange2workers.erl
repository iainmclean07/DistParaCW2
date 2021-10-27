-module(totientrange2workers).
-import(erl_pp, [expr/1]).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
	 sumTotient/0,
     watcher/4,
     start_server/0
	]).

%% TotientRange.erl - Sequential Euler Totient Function (Erlang Version)
%% compile from the shell: >c(totientrange).
%% run from the shell:     >totientrange:sumTotient(1,1000).

%% Phil Trinder 20/10/2018

%% This program calculates the sum of the totients between a lower and an 
%% upper limit. It is based on earlier work by: Nathan Charles, 
%% Hans-Wolfgang Loidl and Colin Runciman

%% The comments provide (executable) Haskell specifications of the functions

%% hcf x 0 = x
%% hcf x y = hcf y (rem x y)

hcf(X,0) -> X;
hcf(X,Y) -> hcf(Y,X rem Y).

%% relprime x y = hcf x y == 1

relprime(X,Y) -> 
  V = hcf(X,Y),
  if 
    V == 1 
      -> true;
    true 
      -> false
  end.

%%euler n = length (filter (relprime n) (mkList n))

euler(N) -> 
  RelprimeN = fun(Y) -> relprime(N,Y) end,  
  length (lists:filter(RelprimeN,(lists:seq(1,N)))).

%% Take completion timestamp, and print elapsed time

printElapsed(S,US) ->
  {_, S2, US2} = os:timestamp(),
                       %% Adjust Seconds if completion Microsecs > start Microsecs
  if
    US2-US < 0 ->
      S3 = S2-1,
      US3 = US2+1000000;
    true ->
      S3 = S2,
      US3 = US2
  end,
  io:format("Time taken: ~p Secs, ~p MicroSecs~n",[S3-S,US3-US]).


%%sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])

sumTotient() -> 
    receive
        {Lower, Upper} ->    
            Range = lists:seq(round(Lower), round(Upper)),
            Eulerians = lists:map(fun euler/1, Range),
            io:format("sum: ~p~n", [lists:sum(Eulerians)]),
            server ! {result, lists:sum(Eulerians)}
        end,
    exit(normal).

watcher(Sum, Count, S, US) when Count == 0 ->
    io:format("Sum of totients: ~p~n", [Sum]),
    printElapsed(S,US);
watcher(Sum, Count, S, US) when Count > 0 ->
    process_flag(trap_exit, true),
    receive
        {range, Lower, Upper} ->
            {_, T, TS} = os:timestamp(), 
            io:format("Starting worker1 with ~p, ~p ~n", [Lower, Upper/2]),
            sumWorker1 ! {Lower, Upper/2},
            io:format("Starting worker2 with ~p, ~p ~n", [(Upper/2)+1, Upper]),
            sumWorker2 ! {(Upper/2)+1, Upper},
            watcher(Sum, Count, T, TS);
        {result, Msg} ->
            io:format("Received ~p~n", [Msg]),
            watcher(Sum + Msg, Count-1, S, US);
        {'EXIT', Pid, normal} ->
            io:format("Worker ~p finished", [Pid]),
            ok;
        {'EXIT', sumWorker1, _} ->
            io:format("sumWorker1 failed");
        {'EXIT', sumWorker2, _} ->
            io:format("sumWorker1 failed");
        finished ->
            io:format("Sum = ~p~n", [Sum])
        end,
    watcher(Sum, Count, S, US).

start_server() ->
    register(server, spawn_link(totientrange2workers, watcher, [0, 2, 0, 0])),
    register(sumWorker1, spawn_link(totientrange2workers, sumTotient, [])),
    register(sumWorker2, spawn_link(totientrange2workers, sumTotient, [])).