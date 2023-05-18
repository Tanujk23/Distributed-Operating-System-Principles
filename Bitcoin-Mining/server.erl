-module(server).
-export([
    start/0,
    intialize/1,
    initialize_workers/3,
    worker_node/2,
    connect_client/0,
    do_recv/1,
    start_mining/1,
    begin_mining/1,
    stop/0, stop/1,
    master_node/1
]).

start_mining(Socket) ->
    do_recv(Socket),
    start_mining(Socket).

connect_client() ->
    {ok, LSocket} = gen_tcp:listen(5683, [binary, {packet, 0}, {reuseaddr, true}, {active, false}]),
    {ok, Socket} = gen_tcp:accept(LSocket),
    start_mining(Socket).

initialize_workers(0, Workers, L) ->
    process_flag(trap_exit, true),
    master_node(Workers);
initialize_workers(N, Workers, L) ->
    Worker_Pid = spawn_link(server, worker_node, [N, L]),
    initialize_workers(N - 1, [{N, Worker_Pid} | Workers], L).

start() ->
    statistics(runtime),
    statistics(wall_clock),
    {ok, [K]} = io:fread("Enter the k value ", "~d"),
    spawn(server, connect_client, []),
    spawn(server, intialize, [K]).

intialize(K) ->
    No_of_Processes = erlang:system_info(logical_processors_available),
    io:fwrite("No of processes ~p", [No_of_Processes]),
    Master_Pid = spawn(server, initialize_workers, [No_of_Processes, [], K]),
    register(master, Master_Pid),
    master ! start.

begin_mining(Workers) ->
    lists:foreach(fun({_, PID}) -> PID ! start end, Workers).

stop(Workers) ->
    lists:foreach(fun({_, PID}) -> PID ! die end, Workers).

do_recv(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, ClientMinedData} ->
            io:fwrite("~p~n", [ClientMinedData]),
            do_recv(Socket),
            {_, Runtime} = statistics(runtime),
            {_, WallClock} = statistics(wall_clock),
            io:fwrite(
                "stop workers Runtime is : ~p, ClockTime is : ~p, Rumtime/Clocktime is : ~p", [
                    Runtime, WallClock, Runtime / WallClock
                ]
            );
        {error, closed} ->
            io:fwrite("[C] closed", []);
        Error ->
            io:fwrite("[C] error ~p~n", [Error])
    end.

master_node(Workers) ->
    receive
        {L, {RandomString, HashCode}} ->
            Final_Hashed_String = string:concat(string:concat(RandomString, " "), HashCode),
            io:fwrite("~p~n", [Final_Hashed_String]),
            % {_, Runtime} = statistics(runtime),
            % {_, WallClock} = statistics(wall_clock),
            % io:fwrite(
            %     "stop workers Runtime is : ~p, ClockTime is : ~p, Rumtime/Clocktime is : ~p", [
            %         Runtime, WallClock, Runtime / WallClock
            %     ]
            % ),
            stop(Workers),
            initialize_workers(length(Workers), Workers, L),
            master ! start,
            stop(Workers),
            stop();
        stop ->
            stop(Workers),
            stop();
        start ->
            begin_mining(Workers),
            master_node(Workers)
    end.

worker_node(N, Input) ->
    receive
        die ->
            exit(N);
        start ->
            master ! {Input, utils:getHashCode(Input)},
            worker_node(N, Input)
    end.

stop() ->
    exit(self()).
