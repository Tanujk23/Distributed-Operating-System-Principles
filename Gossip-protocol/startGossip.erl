-module(startGossip).
-export([start/3]).

start(NoOfNodes, Network, Algorithm) ->
  if
    Network == "full" ->
      fullNetwork:start(NoOfNodes, Algorithm);
    Network == "2D" ->
      twodNetwork:start(NoOfNodes, Algorithm);
    Network == "line" ->
      lineNetwork:start(NoOfNodes, Algorithm);
    Network == "imp2D" ->
      imptwodNetwork:start(NoOfNodes, Algorithm);
    true ->
      done
  end.