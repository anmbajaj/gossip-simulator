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
-export([startNode/1, start/3, start_supervisor/1, build_topology/5]).
-import(lists,[append/2]).
-import(string,[concat/2]).

-define(MESSAGE, "Rumor").
-define(GOSSIP, "Gossip").
-define(PUSH_SUM, "Push_Sum").
-define(TWO_D_GRID, "2DGrid").
-define(IMPERFECT_THREE_D_GRID, "Imperfect3DGrid").
-define(SERVER_PREFIX, "Server@").

%% Function to start the server on which gossip simulator runs
startNode(ServerIP) ->
  ServerPrefix = ?SERVER_PREFIX,
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
  io:fwrite("Reached Inside start ~n"),
  if
    Topology == ?TWO_D_GRID ->
      NumActors = nearest_perfect_square(N);
    Topology == ?IMPERFECT_THREE_D_GRID ->
      NumActors = nearest_perfect_cube(N);
    true ->
      NumActors = N
  end,
  io:fwrite("Num actors ~p ~n",[NumActors]),
  if
    Algorithm == ?GOSSIP ->
      ActorPIDs = spawn_gossip_actors_on_node(0, NumActors, []);
    Algorithm == ?PUSH_SUM ->
      ActorPIDs = spawn_push_sum_actors_on_node(1, 1, 0, NumActors+1, []);
    true ->
      ActorPIDs = [],
      ok
  end,
  io:fwrite("... build Topology ~n"),
  statistics(wall_clock),
  statistics(runtime),
  Neighbors = maps:new(),
  NeighborsMap = build_topology('', NumActors, Topology, ActorPIDs, Neighbors),
  PID = spawn(?MODULE, start_supervisor, [NeighborsMap]),
  PID ! {start, Algorithm}.

remove_actor_from_its_neighbor_list(_, [], NeighborsMap) -> NeighborsMap;
remove_actor_from_its_neighbor_list(ActorPID, [Neighbor | Neighbors], NeighborsMap) ->
  NeighborsList = maps:get(Neighbor, NeighborsMap),
  UpdatedNeighborsMap = maps:update(Neighbor, lists:delete(ActorPID, NeighborsList), NeighborsMap),
  remove_actor_from_its_neighbor_list(ActorPID, Neighbors, UpdatedNeighborsMap).

remove_actor_from_map(ActorPID, NeighborsMap) ->
  Neighbors = maps:get(ActorPID, NeighborsMap),
  maps:remove(ActorPID, NeighborsMap),
  remove_actor_from_its_neighbor_list(ActorPID, Neighbors, NeighborsMap).

%% Starting the supervisor that manages the actors
start_supervisor(NeighborsMap) ->
  receive
    {start, Algorithm} ->
      ActorPIDs = maps:keys(NeighborsMap),
      if
        Algorithm == ?GOSSIP ->
          Message = ?MESSAGE,
          spread_rumor(ActorPIDs, Message);
        Algorithm == ?PUSH_SUM ->
          send_push_sum(ActorPIDs);
        true -> ok
      end,
    start_supervisor(NeighborsMap);
    {ActorPID, provide_neighbors, Message} ->
      ActorPID ! {self(), active_neighbors, maps:get(ActorPID, NeighborsMap), Message},
      start_supervisor(NeighborsMap);
    {ActorPID, terminating} ->
      UpdatedNeighborMap = remove_actor_from_map(ActorPID, NeighborsMap),
      start_supervisor(UpdatedNeighborMap);
    {ActorPID, provide_neighbors, SReceived, WReceived} ->
      ActorPID ! {self(), active_neighbors, maps:get(ActorPID, NeighborsMap), SReceived, WReceived},
      start_supervisor(NeighborsMap);
    {ActorPID, push_sum_actor_terminating} ->
      io:fwrite("Actor Terminated ~p ~n", [ActorPID]),
      EndWallClockTime = element(2, statistics(wall_clock)),
      io:fwrite("Time taken to converge ~p ~n~n", [EndWallClockTime])
  end.

%% Spawn the actors on the node
spawn_gossip_actors_on_node(NumberOfActors, NumberOfActors, Acc) -> Acc;
spawn_gossip_actors_on_node(CountOfSpawnedActors, NumberOfActors, Acc) ->
  PID = spawn(gossip, start, [0]),
  spawn_gossip_actors_on_node((CountOfSpawnedActors + 1), NumberOfActors, [PID | Acc]).

%% Spawn the actors on the node
spawn_push_sum_actors_on_node(NumberOfActors, _, _, NumberOfActors, Acc) -> Acc;
spawn_push_sum_actors_on_node(S, W, TC,  NumberOfActors, Acc) ->
  PID = spawn(push_sum, start, [S, W, TC]),
  spawn_push_sum_actors_on_node(S+1, W, TC, NumberOfActors, [PID | Acc]).

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
    Topology == "Imperfect3DGrid" ->
      ListOfNeighbors = build_imperfect_3D_topology(NumNodes, PIDList, PIDList, Neighbors),
      build_topology(topology_built, NumNodes, Topology, PIDList, ListOfNeighbors);
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

%% Build Imperfect 3D Grid topology
build_imperfect_3D_topology(_,[],_,Neighbors) -> Neighbors;
build_imperfect_3D_topology(NumNodes, [PID|PIDList], PIDs, Neighbors) ->
  io:fwrite("NumNodes value is ~p", [NumNodes]),
  N = trunc(math:ceil(math:pow(NumNodes,1/3))),
  io:fwrite("N value is ~p", [N]),
  NSquared = trunc(math:pow(N, 2)),
  Index = get_index(PID, PIDs),
  Page = math:ceil(Index/NSquared),
  PageStart = (Page-1) * NSquared,
  PageEnd = Page * NSquared,
  case PageStart < Index - N  andalso Index - N < PageEnd of
    true ->
      %% top
      L1 = lists:nth(Index - N, PIDs);
    false -> L1 = ""
  end,
  case Index rem N /= 0 of
    true ->
      %% right
      L2 = lists:nth(Index + 1, PIDs);
    false -> L2 = ""
  end,
  case Index rem N /= 1 of
    true ->
      %% left
      L3 = lists:nth(Index - 1, PIDs);
    false -> L3 = ""
  end,
  case PageStart < Index + N  andalso Index + N =< PageEnd of
    true ->
      %% down
      L4 = lists:nth(Index + N, PIDs);
    false -> L4 = ""
  end,
  case Index + NSquared >= 0 andalso Index + NSquared =< NumNodes of
    true ->
      %% back
      L5 = lists:nth(Index + NSquared, PIDs);
    false -> L5 = ""
  end,
  case Index - NSquared > 0 andalso Index - NSquared < NumNodes of
    true ->
      %% front
      L6 = lists:nth(Index - NSquared, PIDs);
    false -> L6 = ""
  end,
  Nlist = [L1, L2, L3, L4, L5, L6],
  FirstDelete = lists:delete([],Nlist),
  SecondDelete = lists:delete([],FirstDelete),
  GridList = lists:delete([],SecondDelete),

  ListRandom = PIDs -- GridList,
  ListForRandom = ListRandom -- [PID],
  RandomIndex = rand:uniform(length(ListForRandom)),
  RandomPID = lists:nth(RandomIndex, ListForRandom),
  FinalList = [RandomPID | GridList],
  M = maps:put(PID, FinalList, Neighbors),
  build_imperfect_3D_topology(NumNodes,PIDList,PIDs,M).

%% Start the gossip protocol in the actors
spread_rumor(ActorPIDs, Message) ->
  RandomIndex = rand:uniform(length(ActorPIDs)),
  PID = lists:nth(RandomIndex,ActorPIDs),
  PID ! {self(), spread_rumor, Message, self()}.

send_push_sum(ActorPIDs) ->
  RandomIndex = rand:uniform(length(ActorPIDs)),
  PID = lists:nth(RandomIndex,ActorPIDs),
  PID ! {self(), push_sum, RandomIndex, 0.5}.

nearest_perfect_square(N) ->
  NSqrt = math:ceil(math:pow(N, 1/2)),
  trunc(NSqrt * NSqrt).

nearest_perfect_cube(N) ->
  NCbrt = math:ceil(math:pow(N, 1/3)),
  io:fwrite("Perfect Cube is ~p", [trunc(NCbrt * NCbrt * NCbrt)]),
  trunc(NCbrt * NCbrt * NCbrt).