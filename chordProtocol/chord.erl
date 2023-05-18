-module(chord).
-export([
    start/2,
    start_network/2,
    listen_task_completion/2,
    node/4,
    get_successor_i/4,
    near_node/3,
    next_node/2,
    prev_node/2
]).

format_num_nodes(NumNodes) ->
    trunc(math:ceil(math:log2(NumNodes))).

randomNode(Node_Id, []) -> Node_Id;
randomNode(_, ExistingNodes) -> lists:nth(rand:uniform(length(ExistingNodes)), ExistingNodes).

add_node_to_chord(CNodes, TotalNumNodes, M, NetworkState) ->
    PersistingHashes = lists:seq(0, TotalNumNodes - 1, 1) -- CNodes,
    Hash = lists:nth(rand:uniform(length(PersistingHashes)), PersistingHashes),
    Pid = spawn(chord, node, [Hash, M, CNodes, dict:new()]),
    [Hash, dict:store(Hash, Pid, NetworkState)].

get_nearest(_, [], MinNode, _, _) ->
    MinNode;

get_nearest(Key, FingerNodeIds, MinNode, MinVal, State) ->
    [First | Rest] = FingerNodeIds,
    Distance = get_ahead_distance(Key, First, dict:fetch(m, State), 0),
    if
        Distance < MinVal ->
            get_nearest(Key, Rest, First, Distance, State);
        true ->
            get_nearest(Key, Rest, MinNode, MinVal, State)
    end.

get_ahead_distance(Key, Key, _, Distance) ->
    Distance;

get_ahead_distance(Key, NodeId, M, Distance) ->
    get_ahead_distance(Key, (NodeId + 1) rem trunc(math:pow(2, M)), M, Distance + 1).

nearest_preceding_node(_, NodeState, 0) ->
    NodeState;
nearest_preceding_node(Id, NodeState, M) ->
    MthFinger = lists:nth(M, dict:fetch(finger_table, NodeState)),

    case
        is_in_range(
            dict:fetch(id, NodeState), Id, dict:fetch(node, MthFinger), dict:fetch(m, NodeState)
        )
    of
        true ->
            dict:fetch(pid, MthFinger) ! {state, self()},
            receive
                {statereply, FingerNodeState} ->
                    FingerNodeState
            end,
            FingerNodeState;
        _ ->
            nearest_preceding_node(Id, NodeState, M - 1)
    end.

near_node(Key, FingerNodeIds, State) ->
    case lists:member(Key, FingerNodeIds) of
        true -> Key;
        _ -> get_nearest(Key, FingerNodeIds, -1, 10000000, State)
    end.

is_in_range(From, To, Key, M) ->
    if
        From < To ->
            (From =< Key) and (Key =< To);
        trunc(From) == trunc(To) ->
            trunc(Key) == trunc(From);
        From > To ->
            ((Key >= 0) and (Key =< To)) or ((Key >= From) and (Key < trunc(math:pow(2, M))))
    end.

prev_node(Id, NodeState) ->
    case
        is_in_range(
            dict:fetch(id, NodeState) + 1,
            dict:fetch(id, dict:fetch(successor, NodeState)),
            Id,
            dict:fetch(m, NodeState)
        )
    of
        true -> NodeState;
        _ -> prev_node(Id, nearest_preceding_node(Id, NodeState, dict:fetch(m, NodeState)))
    end.

next_node(Id, NodeState) ->
    PredicessorNodeState = prev_node(Id, NodeState),
    dict:fetch(successor, PredicessorNodeState).

node(Hash, M, ChordNodes, _NodeState) ->
    FingerTable = lists:duplicate(M, randomNode(Hash, ChordNodes)),
    NodeStateUpdated = dict:from_list([
        {id, Hash}, {predecessor, nil}, {finger_table, FingerTable}, {next, 0}, {m, M}
    ]),
    node_listen(NodeStateUpdated).

create_nodes(ChordNodes, _, _, 0, NetworkState) ->
    [ChordNodes, NetworkState];

create_nodes(ChordNodes, TotalNodes, M, NumNodes, NetworkState) ->
    [Hash, NewNetworkState] = add_node_to_chord(ChordNodes, TotalNodes, M, NetworkState),
    create_nodes(lists:append(ChordNodes, [Hash]), TotalNodes, M, NumNodes - 1, NewNetworkState).

get_successor_i(Hash, NetworkState, I, M) ->
    case dict:find((Hash + I) rem trunc(math:pow(2, M)), NetworkState) of
        error ->
            get_successor_i(Hash, NetworkState, I + 1, M);
        _ ->
            (Hash + I) rem trunc(math:pow(2, M))
    end.

get_node_pid(Hash, NetworkState) ->
    case dict:find(Hash, NetworkState) of
        error -> nil;
        _ -> dict:fetch(Hash, NetworkState)
    end.

send_message_to_node(_, [], _) ->
    ok;

send_message_to_node(Key, ChordNodes, NetworkState) ->
    [First | Rest] = ChordNodes,
    Pid = get_node_pid(First, NetworkState),
    Pid ! {lookup, First, Key, 0, self()},
    send_message_to_node(Key, Rest, NetworkState).

send_messages_all_nodes(_, 0, _, _) ->
    ok;

send_messages_all_nodes(ChordNodes, NumRequest, M, NetworkState) ->
    timer:sleep(1000),
    Key = lists:nth(rand:uniform(length(ChordNodes)), ChordNodes),
    send_message_to_node(Key, ChordNodes, NetworkState),
    send_messages_all_nodes(ChordNodes, NumRequest - 1, M, NetworkState).

terminate_all_nodes([], _) ->
    ok;

terminate_all_nodes(ChordNodes, NetworkState) ->
    [First | Rest] = ChordNodes,
    get_node_pid(First, NetworkState) ! {kill},
    terminate_all_nodes(Rest, NetworkState).

listen_task_completion(0, HopsCount) ->
    mainprocess ! {totalhops, HopsCount};
listen_task_completion(NumRequests, HopsCount) ->
    receive
        {completed, _Pid, HopsCountForTask, _Key} ->
            listen_task_completion(NumRequests - 1, HopsCount + HopsCountForTask)
    end.

send_messages_and_kill(ChordNodes, NumNodes, NumRequest, M, NetworkState) ->
    register(
        taskcompletionmonitor, spawn(chord, listen_task_completion, [NumNodes * NumRequest, 0])
    ),

    send_messages_all_nodes(ChordNodes, NumRequest, M, NetworkState),

    TotalHops = getTotalHops(),

    {ok, File} = file:open("./stats.txt", [append]),
    io:format(
        File, "~n Average Hops = ~p   TotalHops = ~p    NumNodes = ~p    NumRequests = ~p  ~n", [
            TotalHops / (NumNodes * NumRequest), TotalHops, NumNodes, NumRequest
        ]
    ),
    io:format("~n Average Hops = ~p   TotalHops = ~p    NumNodes = ~p    NumRequests = ~p  ~n", [
        TotalHops / (NumNodes * NumRequest), TotalHops, NumNodes, NumRequest
    ]),
    terminate_all_nodes(ChordNodes, NetworkState).

start_network(NumNodes, NumRequest) ->
    M = format_num_nodes(NumNodes),
    [ChordNodes, NetworkState] = create_nodes([], round(math:pow(2, M)), M, NumNodes, dict:new()),

    fingertable_send(NetworkState, M),
    send_messages_and_kill(ChordNodes, NumNodes, NumRequest, M, NetworkState).

start(NumNodes, NumRequest) ->
    register(mainprocess, spawn(chord, start_network, [NumNodes, NumRequest])).

finger_table_data(_, _, M, M, FingerList) ->
    FingerList;
finger_table_data(Node, NetworkState, M, I, FingerList) ->
    Hash = element(1, Node),
    Ith_succesor = get_successor_i(Hash, NetworkState, trunc(math:pow(2, I)), M),
    finger_table_data(
        Node,
        NetworkState,
        M,
        I + 1,
        FingerList ++ [{Ith_succesor, dict:fetch(Ith_succesor, NetworkState)}]
    ).

fingertable_data(_, [], FTDict, _) ->
    FTDict;

fingertable_data(NetworkState, NetList, FTDict, M) ->
    [First | Rest] = NetList,
    FingerTables = finger_table_data(First, NetworkState, M, 0, []),
    fingertable_data(NetworkState, Rest, dict:store(element(1, First), FingerTables, FTDict), M).

fingertable_node_pass([], _, _) ->
    ok;

fingertable_node_pass(NodesToSend, NetworkState, FingerTables) ->
    [First | Rest] = NodesToSend,
    Pid = dict:fetch(First, NetworkState),
    Pid ! {fix_fingers, dict:from_list(dict:fetch(First, FingerTables))},
    fingertable_node_pass(Rest, NetworkState, FingerTables).

fingertable_send(NetworkState, M) ->
    FingerTables = fingertable_data(NetworkState, dict:to_list(NetworkState), dict:new(), M),
    fingertable_node_pass(dict:fetch_keys(FingerTables), NetworkState, FingerTables).

node_listen(NodeState) ->
    Hash = dict:fetch(id, NodeState),
    receive
        {lookup, Id, Key, HopsCount, _Pid} ->
            NodeVal = near_node(
                Key, dict:fetch_keys(dict:fetch(finger_table, NodeState)), NodeState
            ),
            UpdatedState = NodeState,
            if
                (Hash == Key) ->
                    taskcompletionmonitor ! {completed, Hash, HopsCount, Key};
                (NodeVal == Key) and (Hash =/= Key) ->
                    taskcompletionmonitor ! {completed, Hash, HopsCount, Key};
                true ->
                    dict:fetch(NodeVal, dict:fetch(finger_table, NodeState)) !
                        {lookup, Id, Key, HopsCount + 1, self()}
            end;
        {kill} ->
            UpdatedState = NodeState,
            exit("received exit signal");
        {state, Pid} ->
            Pid ! NodeState,
            UpdatedState = NodeState;
        {get_successor, Id, Pid} ->
            FoundSeccessor = next_node(Id, NodeState),
            UpdatedState = NodeState,
            {Pid} ! {get_successor_reply, FoundSeccessor};
        {fix_fingers, FingerTable} ->
            UpdatedState = dict:store(finger_table, FingerTable, NodeState)
    end,
    node_listen(UpdatedState).

getTotalHops() ->
    receive
        {totalhops, HopsCount} ->
            HopsCount
    end.
