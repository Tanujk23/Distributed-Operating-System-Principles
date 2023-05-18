-module(endprocess).
-export([endAllProcesses/1]).

endAllProcesses(StartTime) ->
  receive
    over ->
      EndTime = erlang:monotonic_time()/10000,
      RunTime = EndTime - StartTime,
      io:format("total time: ~f milliseconds~n", [RunTime]),
      erlang:halt()
  end,
  endAllProcesses(StartTime).
