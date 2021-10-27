-module(totientNrel).
-import(erl_pp, [expr/1]).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
	 sumTotient/0,
   watcher/3,
   server/2,
   start_server/0, 
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
        {Lower, Upper} ->    
            Range = lists:seq(round(Lower), round(Upper)),
            Eulerians = lists:map(fun euler/1, Range),
            % io:format("sum: ~p~n", [lists:sum(Eulerians)]),
            server ! {result, lists:sum(Eulerians)}
        end,
    exit(normal).

% watcher(Sum, Count, {S, US}) ->
%     io:format("Sum of totients: ~p~n", [Sum]),
%     printElapsed(S,US);
%     % exit(finished);
watcher(Lower, Upper, Count) ->
    process_flag(trap_exit, true),
    % io:format("Starting watcher ~p~n", [Count]),
    Name = workerName(Count),
    % Pid = whereis(Name),
    % if Pid == undefined ->
    %   register(Name, spawn_link(totientNrel, sumTotient, [])),
    %   Name ! {Lower, Upper},
    %   io:format("starting ~p with lower ~p upper ~p~n", [Name, Lower, Upper]);
    % true ->
    %   ok
    % end,
    receive
        {start, Lower, Upper, Count} ->
            io:format("Creating ~p~n", [Name]),
            register(Name, spawn_link(totientNrel, sumTotient, [])),
            Name ! {Lower, Upper};
            % watcher(Lower, Upper, Count);
            % Name = workerName(Count),
            
        {'EXIT', Pid, normal} ->
            ok;
        {'EXIT', From, Reason} ->
            io:format("Worker exiting ~p~n", [{'EXIT', From, Reason}]),
            Name = workerName(Count),
            register(Name, spawn_link(totientNrel, sumTotient, [])),
            io:format("restarting ~p with lower ~p upper ~p~n", [Name, Lower, Upper]),
            Name ! {Lower, Upper};
        finished ->
            % io:format("Sum = ~p~n", [Sum])
            server ! finished,
            done
        end,
    watcher(Lower, Upper, Count).

init(Lower, Upper, Count, Work) when Count == 0 ->
    done;
init(Lower, Upper, Count, Work) ->
    Name = watcherName(Count),
    register(Name, spawn_link(totientNrel, watcher, [0, 0, Count])),
    if 
      Lower+Work >= Upper ->
        % register(Name, spawn_link(totientNrel, watcher, [Lower, Upper, Count])),
        Name ! {start, Lower, Upper, Count};
      true -> 
        % register(Name, spawn_link(totientNrel, watcher, [Lower, Lower+Work, Count])),
        Name ! {start, Lower, Lower+Work}
    end,
    init((Lower+Work)+1, Upper, Count-1, Work).

server(Sum, Count) when Count == 0 ->
  io:format("Sum of totients: ~p~n", [Sum]),
  finished;
server(Sum, Count) ->
  % io:format("Starting Server~n"),
  receive
    {range, Lower, Upper, Threads} ->
        init(Lower, Upper, Threads, Upper/Threads),
        server(0, Threads);
    {result, Msg} ->
      server(Sum + Msg, Count-1)
    % finished ->
    %   ok
    end,
  server(Sum, Count).

start_server() ->
    {_, U, US} = os:timestamp(), 
    register(server, spawn_link(totientNrel, server, [0, 1])).

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