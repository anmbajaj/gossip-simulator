%%%-------------------------------------------------------------------
%%% @author harshini
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2022 5:57 PM
%%%-------------------------------------------------------------------
-module(util).
-author("harshini").

%% API
-export([start/1]).
-define(MAX_RUMORS, 10).


%Starting the actors
start(10) ->
  io:fwrite("Mai marne laga hu goodbye ~p ~n", [self()]),
  exit(self());
start(N) ->
  receive
    {_, start, Message, ListOfNeighbors} ->
      Neighbors = maps:get(self(), ListOfNeighbors),
      RandomIndex = rand:uniform(length(Neighbors)),
      PID = lists:nth(RandomIndex,Neighbors),
      PID ! {self(), start, Message, ListOfNeighbors},
      start(N+1)
    %%{Sender, start, initialize} ->
      %%set_state();
    %%{Sender, start, ListOfNeighbors} ->
      %%ok
  end.

%% Runner function to implement initial logic
%runner( _, _, _) ->
 %% RandomIndex = rand:uniform(length(Neighbors)),
  %%PID = lists:nth(RandomIndex,Neighbors),
  %%PID ! {self(), Message, Neighbors}.
%  ok.


