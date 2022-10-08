%%%-------------------------------------------------------------------
%%% @author harshini
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 05. Oct 2022 2:33 PM
%%%-------------------------------------------------------------------
-module(server).
-author("harshini").

%% API
-export([startNode/1, start/3, start_supervisor/0, build_topology/5]).
-import(lists,[append/2]).
-import(string,[concat/2]).


%% Function to start the server on which gossip simulator runs
startNode(ServerIP) ->
  ServerPrefix = "Server@",
  Server = concat(ServerPrefix,ServerIP),
  net_kernel:start([list_to_atom(Server)]),
  erlang:set_cookie('bajaj.anmol-t.matukumalli'),
  ServerNode = node(),
  ServerNodeString = atom_to_list(ServerNode),
  if
    Server == ServerNodeString ->
      io:fwrite("Server Node Created\n");
    true ->
      io:fwrite("Server Node Creation Failed")
  end.

%% Function to start the application
start(N, Topology, Algorithm) ->
  PID = spawn(?MODULE, start_supervisor, []),
  PID ! {start, N, Topology, Algorithm}.

%% Starting the supervisor that manages the actors
start_supervisor() ->
  receive
    {start, N, Topology, Algorithm} ->
      case Topology == "2DGrid" of
        true ->
          RootOfNumNodes = trunc(math:sqrt(N)),
          Square = trunc(math:pow(RootOfNumNodes,2)),
          %% NumNodes is the new number of nodes
          NumNodes = isPerfectSquare(N, Square);
        false ->
          NumNodes = N
      end,
      ActorPIDs = spawn_actors_on_node(0, NumNodes, []),
      Neighbors = maps:new(),
      ListOfNeighbors = build_topology('', NumNodes, Topology, ActorPIDs, Neighbors),
      statistics(wall_clock),
      statistics(runtime),
      if
        Algorithm == "Gossip" ->
          Message = "Test Rumor",
          start_protocol(ActorPIDs, Message, ListOfNeighbors);
        Algorithm == "Push-Sum" ->
          start_protocol(ActorPIDs, ListOfNeighbors);
        true -> ok
      end;
    {_, protocol_executed} ->
      EndWallClockTime = element(2, statistics(wall_clock)),
      EndRuntime = element(2, statistics(runtime)),
      CpuUtilRatio = EndRuntime/EndWallClockTime,
      io:format("Protocol execution is complete."),
      self() ! {self(), cpu_util, CpuUtilRatio};
    {_, cpu_util, CputilRatio} ->
      io:format("CPU Utilization is ~p ~n", [CputilRatio])
  end.

%% Check if the given number of nodes is a perfect square
isPerfectSquare(NumNodes, NumNodes) -> NumNodes;
isPerfectSquare(NumNodes, Square) ->
  case NumNodes == Square of
    true ->
      isPerfectSquare(NumNodes,Square);
    false ->
      NewNum = trunc(math:sqrt(NumNodes + 1)),
      SquareOfNewNum = trunc(math:pow(NewNum,2)),
      isPerfectSquare(NumNodes + 1, SquareOfNewNum)
  end.

%% Spawn the actors on the node
spawn_actors_on_node(NumberOfActors, NumberOfActors, Acc) -> Acc;
spawn_actors_on_node(CountOfSpawnedActors, NumberOfActors, Acc) ->
  PID = spawn(util, start, []),
  spawn_actors_on_node((CountOfSpawnedActors + 1), NumberOfActors, [PID | Acc]).


%% Creating the topologies
build_topology(topology_built, _, _,_,Neighbors) -> Neighbors;
build_topology(_, NumNodes, Topology, PIDList, Neighbors) ->
  if
    Topology == "Full"->
      ListOfNeighbors = build_full_topology(PIDList, PIDList, Neighbors),
      build_topology(topology_built, NumNodes, Topology, PIDList, ListOfNeighbors);
    Topology == "2DGrid" ->
      ListOfNeighbors = build_2D_topology(NumNodes, PIDList, PIDList, Neighbors),
      build_topology(topology_built, NumNodes, Topology, PIDList, ListOfNeighbors);
    Topology == "Line" ->
      ListOfNeighbors = build_line_topology(PIDList, PIDList, Neighbors),
      build_topology(topology_built, NumNodes, Topology, PIDList, ListOfNeighbors);
    %%Topology == "Imperfect3DGrid" ->
      %%build_imperfect_3D_topology(N, PIDList, Neighbors);
    true -> ok
  end.


%% Building full topology
build_full_topology([],_,Neighbors) -> Neighbors;
build_full_topology([PID|PIDList], PIDs, Neighbors) ->
  M = maps:put(PID, PIDs -- [PID], Neighbors),
  build_full_topology(PIDList, PIDs, M).

%% Building line topology
build_line_topology([],_,Neighbors) -> Neighbors;
build_line_topology([PID|PIDList], PIDs, Neighbors) ->
  Index = get_index(PID, PIDs),
  if
    Index == 1 ->
      Nlist = [lists:nth(Index + 1, PIDs)];
    Index == length(PIDs) ->
      Nlist = [lists:nth(Index - 1, PIDs)];
    true ->
      Nlist = [lists:nth(Index - 1, PIDs), lists:nth(Index + 1, PIDs)]
  end,
  M = maps:put(PID, Nlist, Neighbors),
  build_line_topology(PIDList,PIDs,M).

%% Function to get the index of an item in a list
get_index(Item, List) -> get_index(Item, List, 1).

get_index(_,[],_) -> not_found;
get_index(Item, [Item|_], Index) -> Index;
get_index(Item, [_|List], Index) -> get_index(Item, List, Index + 1).


%% Building 2D Grid topology
build_2D_topology(_,[],_,Neighbors) -> Neighbors;
build_2D_topology(NumNodes, [PID|PIDList], PIDs, Neighbors) ->
  N = trunc(math:sqrt(NumNodes)),
  Index = get_index(PID, PIDs),
  case Index - N > 0 andalso Index - N < NumNodes of
    true ->
      L1 = lists:nth(Index - N, PIDs);
    false -> L1 = ""
  end,
  case Index rem N /= 0 of
    true -> L2 = lists:nth(Index + 1, PIDs);
    false -> L2 = ""
  end,
  case Index rem N /= 1 of
    true -> L3 = lists:nth(Index - 1, PIDs);
    false -> L3 = ""
  end,
  case Index + N >= 0 andalso Index + N =< NumNodes of
    true -> L4 = lists:nth(Index + N, PIDs);
    false -> L4 = ""
  end,
  Nlist = [L1, L2, L3, L4],
  FirstDelete = lists:delete([],Nlist),
  FinalList = lists:delete([],FirstDelete),
  M = maps:put(PID, FinalList, Neighbors),
  build_2D_topology(NumNodes,PIDList,PIDs,M).


%% Start the gossip protocol in the actors
start_protocol(ActorPIDs, Message, ListOfNeighbors) ->
  RandomIndex = rand:uniform(length(ActorPIDs)),
  PID = lists:nth(RandomIndex,ActorPIDs),
  PID ! {self(), start, Message, ListOfNeighbors}.

%% Start the push-sum protocol in the actors
start_protocol(ActorPIDs, ListOfNeighbors) ->
  initialize_protocol(ActorPIDs),
  RandomIndex = rand:uniform(length(ActorPIDs)),
  PID = lists:nth(RandomIndex,ActorPIDs),
  PID ! {self(), start, ListOfNeighbors}.

%% Setting states in all actors
initialize_protocol([]) -> true;
initialize_protocol([PID | PIDs]) ->
  PID ! {self(), start, initialize},
  initialize_protocol(PIDs).




