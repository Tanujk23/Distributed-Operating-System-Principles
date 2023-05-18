-module(lineNetwork).
-export([start/2, getRandNeighbor/1]).
-export([doGossip/1, doPushSum/1]).
-export([spawnGossipNodes/1, spawnPushSumNodes/1]).
-export([gossipNodes/1, pushSumNodes/3]).

start(NoOfNodes, Algorithm) ->
  if
    Algorithm == "gossip" ->
      spawnGossipNodes(NoOfNodes),
      doGossip(NoOfNodes);
    Algorithm == "push-sum" ->
      spawnPushSumNodes(NoOfNodes),
      doPushSum(NoOfNodes)
  end.

getRandNeighbor(Num) ->
  random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),
  random:uniform(Num).

gossipNodes(ReceivedTimeOfRumors) ->
  receive
    {rumors, Position} ->
      io:format("Recevied the Rumour at Node ~p~n", [Position]),

      if
        ReceivedTimeOfRumors == 10 ->
          whereis(endprocess) ! over;
        true ->
          done
      end,

      Rand = getRandNeighbor(2),
      if
        Rand == 2 ->
          DesPosition = Position + 1;
        true ->
          DesPosition = Position - 1
      end,

      NeighborName = list_to_atom("gossipNodes" ++ integer_to_list(DesPosition)),
      NeighborPid = whereis(NeighborName),
      io:format("sending to node ~p~n", [DesPosition]),
      NeighborPid ! {rumors, DesPosition},
      gossipNodes(ReceivedTimeOfRumors + 1)
  end.

pushSumNodes(S, W, Rounds) ->
  if
    Rounds == 3 ->
      whereis(endprocess) ! over;
    true ->
      done
  end,

  receive
    {rumors, S_send, W_send, Position} ->
      io:format("Recevied the Rumour at Node ~p~n", [Position]),
      Ratio = (S + S_send)/(W + W_send) - S / W,
      io:format("~p~n", [abs(Ratio)]),

      io:format("~p~n", [Rounds]),

      Rand = getRandNeighbor(2),
      if
        Rand == 2 ->
          DesPosition = Position + 1;
        true ->
          DesPosition = Position - 1
      end,

      NeighborName = list_to_atom("pushSumNodes" ++ integer_to_list(DesPosition)),
      NeighborPid = whereis(NeighborName),
      io:format("sending to node ~p~n", [DesPosition]),
      NeighborPid ! {rumors, S / 2, W / 2, DesPosition},

      if
        abs(Ratio) < 0.0000000001 ->
          pushSumNodes((S + S_send) / 2, (W + W_send) / 2, Rounds + 1);
        true ->
          pushSumNodes((S + S_send) / 2, (W + W_send) / 2, 0)
      end
  end.

doGossip(NoOfNodes) ->
  StartTime = erlang:monotonic_time()/10000,
  register(endprocess, spawn(endprocess, endAllProcesses, [StartTime])),

  RumorStarter = random:uniform(NoOfNodes),
  Name = list_to_atom("gossipNodes" ++ integer_to_list(RumorStarter)),
  Pid = whereis(Name),
  Pid ! {rumors, RumorStarter}.

doPushSum(NoOfNodes) ->
  StartTime = erlang:monotonic_time()/10000,
  register(endprocess, spawn(endprocess, endAllProcesses, [StartTime])),

  RumorStarter = random:uniform(NoOfNodes),
  Name = list_to_atom("pushSumNodes" ++ integer_to_list(RumorStarter)),
  Pid = whereis(Name),
  Pid ! {rumors, 0, 1, RumorStarter}.

spawnGossipNodes(0) ->
  done;
spawnGossipNodes(NoOfNodes) when NoOfNodes > 0 ->
  Pid = spawn(lineNetwork, gossipNodes, [0]),
  Name = list_to_atom("gossipNodes" ++ integer_to_list(NoOfNodes)),
  register(Name, Pid),
  spawnGossipNodes(NoOfNodes - 1).

spawnPushSumNodes(0) ->
  done;
spawnPushSumNodes(NoOfNodes) when NoOfNodes > 0 ->
  Pid = spawn(lineNetwork, pushSumNodes, [NoOfNodes, 1, 0]),
  Name = list_to_atom("pushSumNodes" ++ integer_to_list(NoOfNodes)),
  register(Name, Pid),
  spawnPushSumNodes(NoOfNodes - 1).