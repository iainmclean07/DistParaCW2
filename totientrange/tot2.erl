-module(tot2).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
	 sumTotient/2,
   watcher/1
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
  io:format("Time taken in Secs, MicroSecs ~p ~p~n",[S3-S,US3-US]).


watcher([First | Rest]) ->
  process_flag(trap_exit, true),
  PID = spawn_link(tot2, euler, [First]),
  receive
    {'EXIT', PID, Sum, normal} ->
      watcher([Rest]),
      io:format("completed ~p ~p ~n", [PID, Sum]);
    {'EXIT', PID, _} ->
      watcher([First | Rest]);
    finished ->
      io:format("Watcher finished ~n")
  end.


%%sumTotient lower upper = sum (map euler [lower, lower+1 .. upper])

sumTotient(Lower,Upper) -> 
  {_, S, US} = os:timestamp(),
  % Res = lists:sum(lists:map(fun euler/1,lists:seq(Lower, Upper))),
  PID = spawn_link(tot2, watcher, [lists:seq(Lower, Upper, 10) ++ Upper]),
  register(watcherfun, PID),
  % io:format("Sum of totients: ~p~n", [Res]),
  receive
    finished ->
      printElapsed(S,US)
  end.