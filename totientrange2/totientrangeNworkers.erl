-module(totientrangeNworkers).
-import(erl_pp, [expr/1]).
-export([hcf/2,
 	 relprime/2,
	 euler/1,
	 sumTotient/0,
     watcher/3,
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

workerName(Num) ->
    list_to_atom( "worker" ++ integer_to_list( Num )).

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

watcher(Sum, Count, {S, US}) when Count == 0 ->
    io:format("Sum of totients: ~p~n", [Sum]),
    printElapsed(S,US);
    % exit(finished);
watcher(Sum, Count, {S, US}) when Count > 0 ->
    process_flag(trap_exit, true),
    receive
        {range, Lower, Upper, Threads} ->
            % sumWorker1 ! {Lower, Upper/2},
            % sumWorker2 ! {(Upper/2)+1, Upper};
            {_, T, TS} = os:timestamp(), 
            init(Lower, Upper, Threads, Upper/Threads),
            watcher(Sum, Threads, {T, TS});
        {result, Msg} ->
            % io:format("Sum = ~p Msg = ~p~n", [Sum, Msg]),
            watcher(Sum + Msg, Count-1, {S, US});
        {'EXIT', _, normal} ->
            % io:format("Worker ~p finished~n", [Pid]),
            ok;
        {'EXIT', From, Reason} ->
            io:format("Worker exiting ~p~n", [{'EXIT', From, Reason}]);
        % {'EXIT', sumWorker2, _} ->
        %     io:format("sumWorker1 failed~n");
        finished ->
            io:format("Sum = ~p~n", [Sum])
        end,
    watcher(Sum, Count, {S, US}).

init(_, _, Count, _) when Count == 0 ->
    done;
init(Lower, Upper, Count, Work) ->
    Name = workerName(Count),
    register(Name, spawn_link(totientrangeNworkers, sumTotient, [])),
    
    if 
      Lower+Work >= Upper ->
        % io:format("~p started with Lower = ~p and Upper = ~p~n", [Name, Lower, Upper]),
        Name ! {Lower, Upper};
      true -> 
        % io:format("~p started with Lower = ~p and Upper = ~p~n", [Name, Lower, Lower + Work]),
        Name ! {Lower, Lower+Work}
    end,
    init((Lower+Work)+1, Upper, Count-1, Work).

start_server() ->
    PID = spawn_link(totientrangeNworkers, watcher, [0, 1, {0, 0}]),
    register(server, PID).
    % register(sumWorker1, spawn_link(tot, sumTotient, [])),
    % register(sumWorker2, spawn_link(tot, sumTotient, [])).