%%%-------------------------------------------------------------------
%%% @author harshini
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2022 5:57 PM
%%%-------------------------------------------------------------------
-module(gossip).
-author("harshini").

%% API
-export([start/1]).
-define(MAX_RUMORS_RECEIVED, 10).


%Starting the actors
start(N) ->
  receive
    {SupervisorActor, spread_rumor, Message, Sender} ->
      if N+1 == ?MAX_RUMORS_RECEIVED ->
        io:fwrite("Actor ~p terminating~n", [self()]),
        SupervisorActor ! {self(), terminating},
        exit(self());
        true ->
          SupervisorActor ! {self(), provide_neighbors, Message},
          if Sender == self() ->
            start(N);
            true ->
              start(N+1)
          end,
          ok
      end;
    {SupervisorActor, active_neighbors, Neighbors, Message} ->
      if length(Neighbors) == 0 ->
        io:fwrite("Actor ~p terminating~n", [self()]),
        SupervisorActor ! {self(), terminating},
        exit(self());
        true ->
          ok
      end,
      PID = lists:nth(rand:uniform(length(Neighbors)),Neighbors),
      PID ! {SupervisorActor, spread_rumor, Message, self()},
      self() ! {SupervisorActor, spread_rumor,Neighbors,Message},
      start(N)
  end.