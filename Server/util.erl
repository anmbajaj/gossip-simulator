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
-export([start/0]).
-define(MAX_RUMORS, 10).


%Starting the actors
start() ->
  receive
    {Sender, start, Message, ListOfNeighbors} ->
      runner(Sender, Message, ListOfNeighbors)
    %%{Sender, start, initialize} ->
      %%set_state();
    %%{Sender, start, ListOfNeighbors} ->
      %%ok
  end,
  start().

%% Runner function to implement initial logic
runner( _, _, _) ->
 %% RandomIndex = rand:uniform(length(Neighbors)),
  %%PID = lists:nth(RandomIndex,Neighbors),
  %%PID ! {self(), Message, Neighbors}.
  ok.


