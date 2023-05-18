-module(client).
-export([start/1, start_mining/2]).
-export([init_workers/4, worker_node/2, master_node/2, begin_mining/1, init/2]).
-define(T, #{socket}).

master_node(Workers, Socket) ->
    receive
        {L, {RandomString, HashCode}} ->
            Final_String = string:concat(string:concat(RandomString, " "), HashCode),
            io:fwrite("Sending (~p) the message: ~p~n", [Socket, Final_String]),
            gen_tcp:send(Socket, Final_String),
            stop(Workers),
            init_workers(length(Workers), Workers, L, Socket),
            master ! start,
            stop(Workers),
            stop();
        stop ->
            stop(Workers),
            stop();
        start ->
            begin_mining(Workers),
            master_node(Workers, Socket)
    end.

worker_node(N, Input) ->
    receive
        die ->
            exit(N);
        start ->
            master ! {Input, utils:getHashCode(Input)},
            worker_node(N, Input)
    end.

begin_mining(Workers) ->
    lists:foreach(fun({_, PID}) -> PID ! start end, Workers).

stop(Workers) ->
    lists:foreach(fun({_, PID}) -> PID ! die end, Workers).

stop() ->
    exit(self()).

init_workers(0, Workers, L, Socket) ->
    process_flag(trap_exit, true),
    master_node(Workers, Socket);

init_workers(N, Workers, L, Socket) ->
    Worker_Pid = spawn_link(client, worker_node, [N, L]),
    init_workers(N - 1, [{N, Worker_Pid} | Workers], L, Socket).

init(Socket, L) ->
    NoProcess = erlang:system_info(logical_processors_available),
    Master_Pid = spawn(client, init_workers, [NoProcess, [], L, Socket]),
    register(master, Master_Pid),
    master ! start.

start_mining(Socket, L) ->
    init(Socket, L).

start(IpAddress) ->
    io:fwrite(IpAddress),
    {ok, Tuple1} = inet:parse_address(IpAddress),
    {ok, Socket} = gen_tcp:connect(Tuple1, 5683, [binary, {packet, 0}]),
    start_mining(Socket, 4).
