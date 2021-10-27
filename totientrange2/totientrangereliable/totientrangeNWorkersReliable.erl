-module(totientrangeNWorkersReliable).
-import(erl_pp, [expr/1]).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
	 sumTotient/0,
   watcher/3,
   start_server/0,
   server/3,
   testRobust/2
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

workerName(Num) ->
    list_to_atom( "worker" ++ integer_to_list( Num )).

watcherName(Num) ->
    list_to_atom( "watcher" ++ integer_to_list( Num )).

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
        {Lower, Upper, ID} ->    
            Range = lists:seq(round(Lower), round(Upper)),
            Eulerians = lists:map(fun euler/1, Range),
            % io:format("Eulers ~p~n", [Eulerians]),
            server ! {result, lists:sum(Eulerians)},
            watcherName(ID) ! finished
        end,
    exit(normal).

watcher(Lower, Upper, Threads) ->
    process_flag(trap_exit, true),
    receive
        {start, LowerR, UpperR, ThreadsR} ->
            Name = workerName(ThreadsR),
            io:format("~p calculating ~p ~p ~n", [Name, LowerR, UpperR]),
            register(Name, spawn_link(totientrangeNWorkersReliable, sumTotient, [])),
            Name ! {LowerR, UpperR, ThreadsR},
            watcher(LowerR, UpperR, ThreadsR);
        {'EXIT', _, normal} ->
            ok;
        {'EXIT', From, Reason} ->
            io:format("Worker exiting ~p~n", [{'EXIT', From, Reason}]),
            Name = workerName(Threads),
            register(Name, spawn_link(totientrangeNWorkersReliable, sumTotient, [])),
            io:format("Restarting ~p calculating ~p ~p ~n", [Name, Lower, Upper]),
            Name ! {Lower, Upper, Threads},
            watcher(Lower, Upper, Threads);
        finished ->
            exit(normal)
        end,
    watcher(Lower, Upper, Threads).

init(_, _, _, Threads) when Threads == 0 ->
    done;
init(Lower, Upper, Work, Threads) when Threads > 0 ->
    Name = watcherName(Threads),
    register(Name, spawn_link(totientrangeNWorkersReliable, watcher, [0,0, Threads])),
    if
        Lower + Work > Upper ->
            io:format("Starting ~p with ~p ~p ~n", [Name, Lower, Upper]),
            Name ! {start, Lower, Upper, Threads};
        true ->
            io:format("Starting ~p with ~p ~p ~n", [Name, Lower, Lower+Work]),
            Name ! {start, Lower, Lower+Work, Threads}
        end,
    init(Lower + Work+1, Upper, Work, Threads - 1).
    
server(Sum, Count, {S, US}) when Count == 0 ->
    printElapsed(S, US),
    io:format("Sum of Totients ~p~n", [Sum]);
server(Sum, Count, {S, US}) when Count > 0->
    receive
    {range, Lower, Upper, Threads} ->
        {_, T, TS} = os:timestamp(), 
        init(Lower, Upper, Upper/Threads, Threads),
        server(Sum, Threads, {T, TS});
    {result, Msg} ->
        io:format("Received ~p~n", [Msg]),
        server(Sum + Msg, Count-1, {S, US})
    end,
    server(Sum, Count, {S, US}).
    

start_server() ->
    register(server, spawn_link(totientrangeNWorkersReliable, server, [0, 1, {0, 0}])).

workerChaos(NVictims,NWorkers) ->
  lists:map(
    fun( _ ) ->
      timer:sleep(500),                  %% Sleep for .5s
                                         %% Choose a random victim
      WorkerNum = rand:uniform(NWorkers),     
      io:format("workerChaos killing ~p~n", 
                [workerName(WorkerNum)]),
      WorkerPid = whereis(workerName(WorkerNum)),
      if                                 %% Check if victim is alive
        WorkerPid == undefined ->
          io:format("workerChaos already dead: ~p~n",
                    [workerName(WorkerNum)]);
        true ->                               %% Kill Kill Kill
          exit(whereis(workerName(WorkerNum)),chaos)
      end
    end,
    lists:seq( 1, NVictims ) ).

testRobust(NWorkers,NVictims) ->
  ServerPid = whereis(server),
  if ServerPid == undefined ->
      start_server();
    true ->
      ok
    end,
  server ! {range, 1, 15000, NWorkers},
  workerChaos(NVictims,NWorkers).