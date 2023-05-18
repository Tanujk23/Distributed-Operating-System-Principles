-module(server).
-import(maps, []).
-export[start/0].

start() ->
    io:fwrite("\n\n Hello!!, I am The Twitter Clone \n\n"),
    Table = ets:new(messages, [ordered_set, named_table, public]),
    Map_ClinetToSocket = ets:new(clients, [ordered_set, named_table, public]),
    {ok, SocketListen} = gen_tcp:listen(1204, [binary, {keepalive, true}, {reuseaddr, true}, {active, false}]),
    waitforconnections(SocketListen, Table, Map_ClinetToSocket).

waitforconnections(Listen, Table, Map_ClinetToSocket) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    ok = gen_tcp:send(Socket, "YIP"),
    spawn(fun() -> waitforconnections(Listen, Table, Map_ClinetToSocket) end),
    do_recv(Socket, Table, [], Map_ClinetToSocket).

do_recv(Socket, Table, Bs, Map_ClinetToSocket) ->
    io:fwrite("Do Receive\n\n"),
    case gen_tcp:recv(Socket, 0) of
        {ok, Data1} ->
            
            Data = re:split(Data1, ","),
            Type = binary_to_list(lists:nth(1, Data)),

            io:format("\n\nDATA: ~p\n\n ", [Data]),
            io:format("\n\nTYPE: ~p\n\n ", [Type]),

            if 
                Type == "register" ->
                    UserName = binary_to_list(lists:nth(2, Data)),
                    PID = binary_to_list(lists:nth(3, Data)),
                    io:format("\nPID:~p\n", [PID]),
                    io:format("\nSocket:~p\n", [Socket]),
                    io:format("Type: ~p\n", [Type]),
                    io:format("\n~p wants to register an account\n", [UserName]),
                    
                    Output = ets:lookup(Table, UserName),
                    io:format("Output: ~p\n", [Output]),
                    if
                        Output == [] ->

                            ets:insert(Table, {UserName, [{"followers", []}, {"tweets", []}]}),      
                            ets:insert(Map_ClinetToSocket, {UserName, Socket}),                
                            Temp_List = ets:lookup(Table, UserName),
                            io:format("~p", [lists:nth(1, Temp_List)]),

                          
                            ok = gen_tcp:send(Socket, "User registration succesfull"), % RESPOND BACK - YES/NO
                            io:fwrite("Good to go, Key is not in database\n");
                        true ->
                            ok = gen_tcp:send(Socket, "This Username is already registered ! Try with a different name"),
                            io:fwrite("Duplicate key!\n")
                    end,
                    do_recv(Socket, Table, [UserName], Map_ClinetToSocket);

                Type == "tweet" ->
                    UserName = binary_to_list(lists:nth(2, Data)),
                    Tweet = binary_to_list(lists:nth(3, Data)),
                    io:format("\n ~p sent the following tweet: ~p", [UserName, Tweet]),
                    
                  
                    Value = ets:lookup(Table, UserName),
                    io:format("Output: ~p\n", [Value]),
                    Value3 = lists:nth(1, Value),
                    Value2 = element(2, Value3),
                    Value1 = maps:from_list(Value2),
                    {ok, CurrentFollowers} = maps:find("followers",Value1),                         
                    {ok, CurrentTweets} = maps:find("tweets",Value1),

                    NewTweets = CurrentTweets ++ [Tweet],
                    io:format("~p~n",[NewTweets]),
                    
                    ets:insert(Table, {UserName, [{"followers", CurrentFollowers}, {"tweets", NewTweets}]}),

                    Output_After_Tweet = ets:lookup(Table, UserName),
                    io:format("\nOutput after tweeting: ~p\n", [Output_After_Tweet]),
                  
                    sendMessage(Socket, Map_ClinetToSocket, Tweet, CurrentFollowers, UserName),
                    ok = gen_tcp:send(Socket, "Server processed tweet\n"),
                    do_recv(Socket, Table, [UserName], Map_ClinetToSocket);

                Type == "retweet" ->
                    Person_UserName = binary_to_list(lists:nth(2, Data)),
                    UserName = binary_to_list(lists:nth(3, Data)),
                    Sub_User = string:strip(Person_UserName, right, $\n),
                    io:format("User to retweet from: ~p\n", [Sub_User]),
                    Tweet = binary_to_list(lists:nth(4, Data)),
                    Out = ets:lookup(Table, Sub_User),
                    if
                        Out == [] ->
                            io:fwrite("User does not exist!, Please enter a registered username.\n");
                        true ->
                            % Current User
                            Out1 = ets:lookup(Table, UserName),
                            Value3 = lists:nth(1, Out1),
                            Value2 = element(2, Value3),
                            Value1 = maps:from_list(Value2),
                            % User we are retweeting from
                            Value_3 = lists:nth(1, Out),
                            Value_2 = element(2, Value_3),
                            Value_1 = maps:from_list(Value_2),
                            % current user
                            {ok, CurrentFollowers} = maps:find("followers",Value1),
                            % user we are retweeting from
                            {ok, CurrentTweets} = maps:find("tweets",Value_1),
                            io:format("Tweet to be re-posted: ~p\n", [Tweet]),
                            CheckTweet = lists:member(Tweet, CurrentTweets),
                            if
                                CheckTweet == true ->
                                    NewTweet = string:concat(string:concat(string:concat("re:",Sub_User),"->"),Tweet),
                                    sendMessage(Socket, Map_ClinetToSocket, NewTweet, CurrentFollowers, UserName);
                                true ->
                                    io:fwrite("Tweet does not exist!\n")
                            end     
                    end,
                    io:format("\n ~p wants to retweet something\n", [UserName]),
                    ok = gen_tcp:send(Socket, "Server processed retweet\n"),
                    do_recv(Socket, Table, [UserName], Map_ClinetToSocket);

                Type == "subscribe" ->
                    UserName = binary_to_list(lists:nth(2, Data)),
                    SubscribedUserName = binary_to_list(lists:nth(3, Data)),
                    Sub_User = string:strip(SubscribedUserName, right, $\n),
                    Output1 = ets:lookup(Table, Sub_User),
                    if
                        Output1 == [] ->
                            io:fwrite("The username entered doesn't exist! Please try with a registered username. \n");
                        true ->

                            Value = ets:lookup(Table, Sub_User),
                            Value3 = lists:nth(1, Value),
                            Value2 = element(2, Value3),

                            Value1 = maps:from_list(Value2),                            
                            {ok, CurrentFollowers} = maps:find("followers",Value1),
                            {ok, CurrentTweets} = maps:find("tweets",Value1),

                            NewFollowers = CurrentFollowers ++ [UserName],
                            io:format("~p~n",[NewFollowers]),
                        
                            ets:insert(Table, {Sub_User, [{"followers", NewFollowers}, {"tweets", CurrentTweets}]}),

                            Output2 = ets:lookup(Table, Sub_User),
                            io:format("\nOutput after subscribing: ~p\n", [Output2]),

                            ok = gen_tcp:send(Socket, "Subscribed!"),

                            do_recv(Socket, Table, [UserName], Map_ClinetToSocket)
                    end,
                    io:format("\n ~p wants to subscribe to ~p\n", [UserName, Sub_User]),
                    
                    ok = gen_tcp:send(Socket, "Server processed subscription. Subscribed!"),
                    do_recv(Socket, Table, [UserName], Map_ClinetToSocket);

                Type == "query" ->
                    Option = binary_to_list(lists:nth(3, Data)),
                    UserName = binary_to_list(lists:nth(2, Data)),
                    io:format("Query: The current username is -> ~p\n", [UserName]),
                    % Query = binary_to_list(lists:nth(3, Data)),
                    if
                        Option == "1" ->
                            io:fwrite("My mentions!\n"),
                            MyUserName = binary_to_list(lists:nth(4, Data)),
                            Sub_UserName = ets:first(Table),
                            Sub_User = string:strip(Sub_UserName, right, $\n),
                            io:format("Sub_UserName: ~p\n", [Sub_User]),
                            Tweets = searchAllTweets("@", Table, Sub_User, MyUserName , []),
                            ok = gen_tcp:send(Socket, Tweets);
                        Option == "2" ->
                            io:fwrite("Hashtag Search\n"),
                            Hashtag = binary_to_list(lists:nth(4, Data)),
                            Sub_UserName = ets:first(Table),
                            Sub_User = string:strip(Sub_UserName, right, $\n),
                            io:format("Sub_UserName: ~p\n", [Sub_User]),
                            Tweets = searchAllTweets("#", Table, Sub_User, Hashtag , []),
                            ok = gen_tcp:send(Socket, Tweets);
                        true ->
                            io:fwrite("Subscribed User Search\n"),
                            % Sub_UserName = binary_to_list(lists:nth(4, Data)),
                            Sub_UserName = ets:first(Table),
                            Sub_User = string:strip(Sub_UserName, right, $\n),
                            io:format("Sub_UserName: ~p\n", [Sub_User]),
                            Value = ets:lookup(Table, Sub_User),
                            % io:format("~p~n",[Value]),
                            Value3 = lists:nth(1, Value),
                            Value2 = element(2, Value3),
                            Value1 = maps:from_list(Value2),                            
                            {ok, CurrentTweets} = maps:find("tweets",Value1),
                            io:format("\n ~p : ", [Sub_User]),
                            io:format("~p~n",[CurrentTweets]),
                            searchWholeTable(Table, Sub_User, UserName),
                            ok = gen_tcp:send(Socket, CurrentTweets)
                    end,
                    io:format("\n ~p wants to query", [UserName]),
                    do_recv(Socket, Table, [UserName], Map_ClinetToSocket);
                true ->
                    io:fwrite("\n Anything else!")
            end;

        {error, closed} ->
            {ok, list_to_binary(Bs)};
        {error, Reason} ->
            io:fwrite("error"),
            io:fwrite(Reason)
    end.

searchAllTweets(Symbol, Table, Key, Word, Found) ->
    Search = string:concat(Symbol, Word),
    io:format("Word to be searched: ~p~n", [Search]),
    if
        Key == '$end_of_table' ->
            io:fwrite("Found tweets: ~p~n", [Found]),
            Found;
        true ->
            io:fwrite("Current Row key: ~p~n", [Key]),
            Value = ets:lookup(Table, Key),
            Value3 = lists:nth(1, Value),
            Value2 = element(2, Value3),
            Value1 = maps:from_list(Value2),                              
            {ok, CurrentTweets} = maps:find("tweets",Value1),
            io:fwrite("CurrentTweets: ~p~n", [CurrentTweets]),
            FilteredTweets = [S || S <- CurrentTweets, string:str(S, Search) > 0],
            io:fwrite("FilteredTweets: ~p~n", [FilteredTweets]),
            Found1 = Found ++ FilteredTweets,
            CurrentRow_Key = ets:next(Table, Key),
            searchAllTweets(Symbol, Table, CurrentRow_Key, Word, Found1)
    end.


searchWholeTable(Table, Key, UserName) ->
    CurrentRow_Key = ets:next(Table, Key),
    Value = ets:lookup(Table, CurrentRow_Key),
    % io:format("~p~n",[Value]),
    Value3 = lists:nth(1, Value),
    Value2 = element(2, Value3),
    Value1 = maps:from_list(Value2),                            
    {ok, CurrentFollowers} = maps:find("followers",Value1),
    IsMember = lists:member(UserName, CurrentFollowers),
    if
        IsMember == true ->
            {ok, CurrentTweets} = maps:find("tweets",Value1),
            io:format("\n ~p : ", [CurrentRow_Key]),
            io:format("~p~n",[CurrentTweets]),
            searchWholeTable(Table, CurrentRow_Key, UserName);
        true ->
            io:fwrite("\n No more tweets!\n")
    end,
    io:fwrite("\n Searching the whole table!\n").

sendMessage(Socket, Map_ClinetToSocket, Tweet, Subscribers, UserName) ->
    if
        Subscribers == [] ->
            io:fwrite("\nNo followers!\n");
        % Map_ClinetToSocket == [] ->
        %     io:fwrite("\nAll clients empty!\n");
        true ->
            % io:format("\nAll Clients: ~p~n",[Map_ClinetToSocket]),

            [Client_To_Send | Remaining_List ] = Subscribers,
            io:format("Client to send: ~p\n", [Client_To_Send]),
            io:format("\nRemaining List: ~p~n",[Remaining_List]),
            Client_Socket_Row = ets:lookup(Map_ClinetToSocket,Client_To_Send),
            Value3 = lists:nth(1, Client_Socket_Row),
            Client_Socket = element(2, Value3),
            io:format("\nClient Socket: ~p~n",[Client_Socket]),
            
            ok = gen_tcp:send(Client_Socket, ["New tweet received! from \n",UserName,":",Tweet]),
            ok = gen_tcp:send(Socket, "Your tweet is now posted\n"),
            
            sendMessage(Socket, Map_ClinetToSocket, Tweet, Remaining_List, UserName)
    end,
    io:fwrite("Send message!\n").


printMap(Map) ->
    io:fwrite("**************\n"),
    List1 = maps:to_list(Map),
    io:format("~s~n",[tuplelist_to_string(List1)]),
    io:fwrite("**************\n").

tuplelist_to_string(L) ->
    tuplelist_to_string(L,[]).

tuplelist_to_string([],Acc) ->
    lists:flatten(["[",
           string:join(lists:reverse(Acc),","),
           "]"]);
tuplelist_to_string([{X,Y}|Rest],Acc) ->
    S = ["{\"x\":\"",X,"\", \"y\":\"",Y,"\"}"],
    tuplelist_to_string(Rest,[S|Acc]).

conn_loop(Socket) ->
    io:fwrite("Uh Oh, I can sense someone trying to connect to me!\n\n"),
    receive
        {tcp, Socket, Data} ->
            io:fwrite("...."),
            io:fwrite("\n ~p \n", [Data]),
            if 
                Data == <<"register_account">> ->
                    io:fwrite("Client wants to register an account"),
                    ok = gen_tcp:send(Socket, "username"), % RESPOND BACK - YES/NO
                    io:fwrite("is now registered");
                true -> 
                    io:fwrite("TRUTH")
            end,
            conn_loop(Socket);
            
        {tcp_closed, Socket} ->
            io:fwrite("I swear I am not here!"),
            closed
    end.