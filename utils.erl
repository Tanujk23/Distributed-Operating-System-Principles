-module(utils).
-export([hash/1, get_random_hash/1, getHashCode/1]).
-export([init_workers/3, worker_node/2, master_node/1, begin_mining/1, init/1, getFinalString/1]).

get_rand(L) -> gen_rnd(L, "abcdefghijklmnopqrstuvwxyz1234567890").

gen_rnd(Length, AllowedChars) ->
    MaxLength = length(AllowedChars),
    lists:foldl(
        fun(_, Acc) -> [lists:nth(rand:uniform(MaxLength), AllowedChars)] ++ Acc end,
        [],
        lists:seq(1, Length)
    ).

hash(N) -> io_lib:format("~64.16.0b", [binary:decode_unsigned(crypto:hash(sha256, N))]).

get_random_string(L) ->
    UFID = "hemanth.bandari;",
    RandomString = get_rand(L),
    string:concat(UFID, RandomString).

get_random_hash(L) ->
    Random = get_random_string(L),
    HashCode = hash(Random),
    {Random, HashCode}.

getZeros(N) ->
    lists:foldl(fun(_, Acc) -> string:concat(Acc, "0") end, "", lists:seq(1, N)).

checkHashCode(HashCode, N) ->
    Bool_value = string:equal(string:slice(HashCode, 0, N), getZeros(N)),
    Check =
        if
            Bool_value -> 1;
            true -> 0
        end,
    Check.

getHashCode(N) ->
    {RandomString, HashCode} = utils:get_random_hash(5),
    Val = checkHashCode(HashCode, N),
    if
        Val == 1 ->
            {RandomString, HashCode};
        true ->
            getHashCode(N)
    end.

getFinalString(N) ->
    {RandomString, HashCode} = getHashCode(N),
    string:concat(string:concat(RandomString, " "), HashCode).

master_node(Workers) ->
    receive
        {PID, {RandomString, HashCode}} ->
            myPro ! {success, RandomString, HashCode},
            stop(Workers),
            stop();
        stop ->
            stop(Workers);
        start ->
            begin_mining(Workers),
            master_node(Workers)
    end.

worker_node(N, Input) ->
    receive
        die ->
            exit(N);
        start ->
            getHashCode(Input),
            worker_node(N, Input)
    end.

begin_mining(Workers) ->
    lists:foreach(fun({_, PID}) -> PID ! start end, Workers).

stop(Workers) ->
    lists:foreach(fun({_, PID}) -> PID ! die end, Workers).

stop() ->
    exit(self()).

init_workers(0, Workers, L) ->
    process_flag(trap_exit, true),
    master_node(Workers);

init_workers(N, Workers, L) ->
    Worker_Pid = spawn_link(utils, worker_node, [N, L]),
    init_workers(N - 1, [{N, Worker_Pid} | Workers], L).

init(L) ->
    register(myPro, self()),
    NoProcess = erlang:system_info(logical_processors_available),
    Master_Pid = spawn(utils, init_workers, [NoProcess, [], L]),
    Master = register(master, Master_Pid),
    master ! start,
    receive
        {success, RandomString, HashCode} ->
            {RandomString, HashCode}
    end.