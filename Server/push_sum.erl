%%%-------------------------------------------------------------------
%%% @author anmol
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 10. Oct 2022 5:05 AM
%%%-------------------------------------------------------------------
-module(push_sum).
-author("anmol").

%% API
-export([start/3]).
-define(MAX_FREQUENCY_OF_MIN_DIFF, 3).


%Starting the actors
start(SOwn, WOwn, TerminationCountOwn) ->
  receive
    {SupervisorActor, push_sum, SReceived, WReceived} ->
      io:fwrite("Received a push sum request for pid ~p~n", [self()]),
      SupervisorActor ! {self(), provide_neighbors, SReceived, WReceived},
      start(SOwn,WOwn,TerminationCountOwn);
    {SupervisorActor, active_neighbors, Neighbors, SReceived, WReceived} ->
      NumNeighbor = length(Neighbors),
      io:fwrite("Neighbor ~p  and NumNeighbor ~p ~n", [Neighbors, NumNeighbor]),
      if NumNeighbor == 0 ->
        io:fwrite("Actor ~p terminating~n", [self()]),
        SupervisorActor ! {self(), push_sum_actor_terminating},
        exit(self());
        true ->
          ok
      end,
      SNew = SOwn + SReceived,
      WNew = WOwn + WReceived,
      Checker = 1/math:pow(10,10),
      io:fwrite("Checker is ~p ~n", [Checker]),
    io:fwrite("SNew/WNew Ratio is ~p SOwn/WOwn ~p and the differnce is ~p ~n", [SNew/WNew, SOwn/WOwn, (SNew/WNew - SOwn/WOwn)]),
      if abs(SNew/WNew - SOwn/WOwn) =< Checker ->
        if TerminationCountOwn+1 == ?MAX_FREQUENCY_OF_MIN_DIFF->
          io:fwrite("Maximum frequency reached Actor ~p terminating~n", [self()]),
          SupervisorActor ! {self(), push_sum_actor_terminating},
          exit(self());
          true ->
            PID = lists:nth(rand:uniform(length(Neighbors)),Neighbors),
            PID ! {SupervisorActor, push_sum, SNew/2, WNew/2},
            %self() ! {SupervisorActor, spread_rumor,Neighbors,Message},
            start(SNew/2, WNew/2, TerminationCountOwn+1)
        end;
        true ->
          PID = lists:nth(rand:uniform(length(Neighbors)),Neighbors),
          PID ! {SupervisorActor, push_sum, SNew/2, WNew/2},
          start(SNew/2, WNew/2, 0)
      end
  end.


%if Sender == self() ->
%start(N);
%true ->
%start(N+1)
%end,
%ok

%if (SNew/WNew - SOwn/WOwn) =< math:pow(10,-10) ->


%start(truc(SNew/2), truc(WNew/2), TerminationCountReceived+1);
%true ->
%start(truc(SNew/2), truc(WNew/2), 0)
%end,
%if length(Neighbors) == 0 ->
%io:fwrite("Actor ~p terminating~n", [self()]),
%SupervisorActor ! {self(), terminating},
%exit(self());
%true ->
%ok
%end,
%PID = lists:nth(rand:uniform(length(Neighbors)),Neighbors),
%PID ! {SupervisorActor, spread_rumor, Message, self()},
%self() ! {SupervisorActor, spread_rumor,Neighbors,Message},
%start(N)



%if TerminationCountReceived == ?MAX_FREQUENCY_OF_MIN_DIFF ->
%io:fwrite("Actor ~p terminating~n", [self()]),
%SupervisorActor ! {self(), terminating},
%exit(self());
%true ->