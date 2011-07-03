processPackage(IP, InPortNo, Packet) ->
    try decodeMsg(Packet) of
        Msg ->
            {name,Sender} = gen_server:call(named,{getName,{IP,InPortNo}}),
            case Msg of
                #goods{} ->
                    warehouse ! Msg;
                #sdoog{} ->
                    warehouse ! Msg;
                #routed{content=ContentMsg} ->
                    case ContentMsg of
                        #peers{} ->
                    routeMsg(Sender,Msg)
            end 
        catch
            _ -> throw(unableToParseMessage)
        end.


    answerPeers(#routed{routeDone=Done,routeTodo=Todo,content=#peers{}}) ->
        {neighs,Neighs} = gen_server:call(named,{getNeighs}),
        NewRoute = lists:reverse(Done),
        [NextHop|_] = NewRoute,
        sendMsg(NextHop,#routed{routeDone=Todo,routeTodo=NewRoute,content=#sreep{peers=Neighs}}).

