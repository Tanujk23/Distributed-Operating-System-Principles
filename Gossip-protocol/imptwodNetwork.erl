-module(imptwodNetwork).
-export([start/2]).
-export([doGossip/1, doPushSum/1]).
-export([loopTwoDGossipNodes/3, loopGetExistingNeighborTwoD/3, startTwoDGossipNodes/1, startTwoDPushSumNodes/1,gossipNodeTwo2D/1]).
-export([getRandNeighbor2D/1, loopTwoDPushSumNodes/4, loopGetExistingNeighborPushSumTwoD/5,pushSumNodesTwoD/4, getRandNeighbor/1]).

start(NoOfNodes, Algorithm) ->
  if
    Algorithm == "gossip" ->
      startTwoDGossipNodes(NoOfNodes),
      doGossip(NoOfNodes);
    Algorithm == "push-sum" ->
      startTwoDPushSumNodes(NoOfNodes),
      doPushSum(NoOfNodes)
  end.

getRandNeighbor(Num) ->
  random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),
  random:uniform(Num).

doGossip(NoOfNodes) ->
  StartTime = erlang:monotonic_time()/10000,
  register(endprocess, spawn(endprocess, endAllProcesses, [StartTime])),

  R = trunc(math:sqrt(NoOfNodes)),
  StartRumour_X = random:uniform(R),
  StartRumour_Y = random:uniform(R),

  Name = list_to_atom("gossipNodeTwo2D" ++ integer_to_list(StartRumour_X) ++ "_" ++ integer_to_list(StartRumour_Y)),
  Pid = whereis(Name),
  Pid ! {rumors, [StartRumour_X, StartRumour_Y], NoOfNodes}.

startTwoDGossipNodes(NoOfNodes) ->
  R = trunc(math:sqrt(NoOfNodes)),
  loopTwoDGossipNodes(R, R, R).

loopTwoDGossipNodes(0, 0, _) ->
  done;
loopTwoDGossipNodes(X, Y, R) ->
  Pid = spawn(imptwodNetwork, gossipNodeTwo2D, [0]),
  Name = list_to_atom("gossipNodeTwo2D" ++ integer_to_list(X) ++ "_" ++ integer_to_list(Y)),
  register(Name, Pid),
  if
    X > 0 ->
      loopTwoDGossipNodes(X - 1, Y, R);
    Y > 0 ->
      loopTwoDGossipNodes(R, Y - 1, R);
    true ->
      done
  end.

gossipNodeTwo2D(ReceivedTimeOfRumors) ->
  receive
    {rumors, [X, Y], N} ->
      io:format("Rumors received!! I'm Node ~p_~p~n", [X, Y]),

      if
        ReceivedTimeOfRumors == 10 ->
          whereis(endprocess) ! over;
        true ->
          done
      end,

      Rand = getRandNeighbor(2),
      if
        Rand == 1 ->
          loopGetExistingNeighborTwoD(X, Y, N);
        true ->
          R = trunc(math:sqrt(N)),
          GoingTo_X = random:uniform(R),
          GoingTo_Y = random:uniform(R),

          Name = list_to_atom("gossipNodeTwo2D" ++ integer_to_list(GoingTo_X) ++ "_" ++ integer_to_list(GoingTo_Y)),
          Pid = whereis(Name),
          Pid ! {rumors, [GoingTo_X, GoingTo_Y], N},
          io:format("sending to node ~p~p~n", [GoingTo_X, GoingTo_Y])
      end,

      gossipNodeTwo2D(ReceivedTimeOfRumors + 1)
  end.

loopGetExistingNeighborTwoD(X, Y, N) ->
  NeighborCoordinate = getRandNeighbor2D([X, Y]),
  Neighbor_X = lists:nth(1, NeighborCoordinate),
  Neighbor_Y = lists:nth(2, NeighborCoordinate),

  NeighborName = list_to_atom("gossipNodeTwo2D" ++ integer_to_list(Neighbor_X) ++ "_" ++ integer_to_list(Neighbor_Y)),
  NeighborPid = whereis(NeighborName),

  if
    is_pid(NeighborPid) ->
      io:format("sending to node ~p~n", [NeighborName]),
      NeighborPid ! {rumors, [Neighbor_X, Neighbor_Y], N},
      done;
    true ->
      loopGetExistingNeighborTwoD(X, Y, N)
  end.

getRandNeighbor2D([X, Y]) ->
  random:seed(erlang:phash2([node()]),
    erlang:monotonic_time(),
    erlang:unique_integer()),

  List = [[0, -1], [0, 1], [-1, 0], [1, 0], [-1 ,-1], [1, 1], [-1, 1], [1, -1]],
  NeighborCoordinate = lists:nth(random:uniform(8), List),
  Neighbor_X = lists:nth(1, NeighborCoordinate) + X,
  Neighbor_Y = lists:nth(2, NeighborCoordinate) + Y,
  [Neighbor_X, Neighbor_Y].

doPushSum(NoOfNodes) ->
  StartTime = erlang:monotonic_time()/10000,
  register(endprocess, spawn(endprocess, endAllProcesses, [StartTime])),

  R = trunc(math:sqrt(NoOfNodes)),
  StartRumour_X = random:uniform(R),
  StartRumour_Y = random:uniform(R),

  Name = list_to_atom("pushSumNodesTwoD" ++ integer_to_list(StartRumour_X) ++ "_" ++ integer_to_list(StartRumour_Y)),
  Pid = whereis(Name),
  Pid ! {rumors, 0, 1, [StartRumour_X, StartRumour_Y], NoOfNodes}.

startTwoDPushSumNodes(NoOfNodes) ->
  io:format("2DPushSumNodes!"),
  R = trunc(math:sqrt(NoOfNodes)),
  loopTwoDPushSumNodes(R, R, R, NoOfNodes).

loopTwoDPushSumNodes(0, 0, _, _) ->
  done;
loopTwoDPushSumNodes(X, Y, R, NoOfNodes) ->
  Pid = spawn(imptwodNetwork, pushSumNodesTwoD, [NoOfNodes, 1, [X, Y], 0]),
  Name = list_to_atom("pushSumNodesTwoD" ++ integer_to_list(X) ++ "_" ++ integer_to_list(Y)),
  register(Name, Pid),
  if
    X > 0 ->
      loopTwoDPushSumNodes(X - 1, Y, R, NoOfNodes - 1);
    Y > 0 ->
      loopTwoDPushSumNodes(R, Y - 1, R, NoOfNodes - 1);
    true ->
      done
  end.

loopGetExistingNeighborPushSumTwoD(X, Y, S, W, N) ->
  NeighborCoordinate = getRandNeighbor2D([X, Y]),
  Neighbor_X = lists:nth(1, NeighborCoordinate),
  Neighbor_Y = lists:nth(2, NeighborCoordinate),

  NeighborName = list_to_atom("pushSumNodesTwoD" ++ integer_to_list(Neighbor_X) ++ "_" ++ integer_to_list(Neighbor_Y)),
  NeighborPid = whereis(NeighborName),

  if
    is_pid(NeighborPid) ->
      io:format("sending to node ~p~n", [NeighborName]),
      NeighborPid ! {rumors, S, W, [Neighbor_X, Neighbor_Y], N},
      done;
    true ->
      loopGetExistingNeighborPushSumTwoD(X, Y, S, W, N)
  end.


pushSumNodesTwoD(S, W, [X, Y], Rounds) ->
  if
    Rounds == 3 ->
      whereis(endprocess) ! over;
    true ->
      done
  end,

  receive
    {rumors, S_send, W_send, [X, Y], N} ->
      io:format("Recevied the Rumour at Node ~p~n", [self()]),
      Ratio = (S + S_send)/(W + W_send) - S / W,
      io:format("~p~n", [abs(Ratio)]),

      io:format("~p~n", [Rounds]),

      Rand = getRandNeighbor(2),
      if
        Rand == 1 ->
          loopGetExistingNeighborPushSumTwoD(X, Y, S, W, N);
        true ->
          R = trunc(math:sqrt(N)),
          GoingTo_X = random:uniform(R),
          GoingTo_Y = random:uniform(R),

          Name = list_to_atom("pushSumNodesTwoD" ++ integer_to_list(GoingTo_X) ++ "_" ++ integer_to_list(GoingTo_Y)),
          Pid = whereis(Name),
          Pid ! {rumors, S, W, [GoingTo_X, GoingTo_Y], N},
          io:format("sending to node ~p~p~n", [GoingTo_X, GoingTo_Y])
      end,


      if
        abs(Ratio) < 0.0000000001 ->
          pushSumNodesTwoD((S + S_send) / 2, (W + W_send) / 2, [X, Y], Rounds + 1);
        true ->
          pushSumNodesTwoD((S + S_send) / 2, (W + W_send) / 2, [X, Y], 0)
      end
  end.