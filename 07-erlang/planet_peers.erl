



answerPeers(#routed{routeDone=Done,routeTodo=Todo,content=#peers{}}) ->
    {neighs,Neighs} = gen_server:call(named,{getNeighs}),
    NewRoute = lists:reverse(Done),
    [NextHop|_] = NewRoute,
    sendMsg(NextHop,#routed{routeDone=Todo,routeTodo=NewRoute,content=#sreep{peers=Neighs}}).

