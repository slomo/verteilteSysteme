-module(planet).
-behaviour(gen_server).

-include("./messages.hrl").

-export([start/2,peers/0,connect/2]).
-export([init/1,handle_call/3,code_change/3,handle_cast/2,handle_info/2,terminate/2]).
-define(UDP_PACKET_LENGTH,200).

% TODO: DEBUG ONLY
-export([decodeMsg/1,encodeMsg/1]).

% -----------------------------------------------------------------------------

-record(state,{socket,named}).

% --------------------------------------------------------------------------



start(Port, Name) ->
    gen_server:start(?MODULE,{Port,Name},[]).

init({Port,Name}) ->
    % start named
    {ok,Named} = gen_server:start(named,Name,[]),
    register(named,Named),
    case gen_udp:open(Port) of
        {ok, Socket} ->
            gen_udp:controlling_process(Socket,self()),
            register(netd,self()),
            {ok,#state{socket=Socket,named=Named}};
        {error, Reason} ->
            log(openSocketFailed,Reason),
            {error,Reason}
    end.

handle_call(Request,_Sender,State = #state{socket = Socket}) ->
    case Request of
        {udp, _Socket, IP, InPortNo, Packet} ->
            processPackage(IP, InPortNo,Packet),
            {noreply,State};
        {send,{Ip,Port},Msg} ->
            String = encodeMsg(Msg),
            gen_udp:send(Socket,Ip,Port,String),
            {reply,ok,State};
        {send,Name,Msg} ->
            String = encodeMsg(Msg),
            {addr,{Ip,Port}} = gen_server:call(named,{getAddr,Name}),
            gen_udp:send(Socket,Ip,Port,String),
            {reply,ok,State}
    end.

handle_cast(Request, State) ->
    log(handel_castUnkonwMEssage,Request),
    {noreply,State}.

handle_info(Info, State) ->
    case Info of
        {udp, _Socket, IP, InPortNo, Packet} ->
            spawn(fun() -> processPackage(IP, InPortNo,Packet) end),
            {noreply,State};
        _ ->
            log(handle_unkonwInfo,Info),
            {noreply,State}
    end.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.

terminate(_Reason,_State = #state{socket= Socket}) ->
    unregister(netd),
    unregister(named),
    gen_udp:close(Socket),
    ok.

% --- send -------------------------------------------------------------------

connect(Ip,Port) -> 
    {myName,MyName} = gen_server:call(named,{getMyName}),
    gen_server:call(netd,{send,{Ip,Port},#hello{name=MyName}}).

sendMsg(NextHop,NextMsg) ->
    gen_server:call(netd,{send,NextHop,NextMsg}).



% --- processing logic --------------------------------------------------------
processPackage(IP, InPortNo, Packet) ->
    try decodeMsg(Packet) of
        #hello{name = Name} ->
            gen_server:call(named,{addEntry,Name,{IP,InPortNo}}),
            {myName,MyName} = gen_server:call(named,{getMyName}),
            gen_server:call(netd,{send,{IP,InPortNo},#olleh{name = MyName}});
        #olleh{name = Name} ->
            gen_server:call(named,{addEntry,Name,{IP,InPortNo}});
        Msg ->
            {name,Sender} = gen_server:call(named,{getName,{IP,InPortNo}}),
            case Msg of
                #routed{} ->
                    routeMsg(Sender,Msg)
            end
    catch
        _ -> throw(unableToParseMessage)
    end.

routeMsg(_Sender,Msg = #routed{routeDone=Done,routeTodo=Todo,content=Content}) ->
    {myName,MyName} = gen_server:call(named,{getMyName}),
    case Todo of
        [MyName] -> % dispatch
            case Content of
                #peers{} ->
                    answerPeers(Msg);
                #sreep{} ->
                    peered ! Msg
            end;
        [MyName|Remain] ->
            NextMsg = #routed{routeTodo=Remain,routeDone = Done ++ [MyName],content=Content},
            [NextHop|_] = Remain,
            gen_server:call(netd,{send,NextHop,NextMsg});
        _ -> throw({invalidRoutedMessage,Msg})
    end.

answerPeers(#routed{routeDone=Done,routeTodo=Todo,content=#peers{}}) ->
    {neighs,Neighs} = gen_server:call(named,{getNeighs}),
    NewRoute = lists:reverse(Done),
    [NextHop|_] = NewRoute,
    sendMsg(NextHop,#routed{routeDone=Todo,routeTodo=NewRoute,content=#sreep{peers=Neighs}}).

% --- pack and unpack ---------------------------------------------------------

% @param Packet raw input String
decodeMsg(Packet) ->
    [Type | Content] = string:tokens(Packet," "),
    case Type of
        "HELLO" ->
            [Name|[]] = Content,
            validateName(Name),
            #hello{name=Name};
        "OLLEH" ->
            [Name|[]] = Content,
            validateName(Name),
            #olleh{name=Name};
        "GOODS" ->
            [Src,Goods] = decodeGoods(Content),
            #goods{src=Src, goods=Goods};
        "SDOOG" ->
            [Src,Goods] = decodeGoods(Content),
            #sdoog{src=Src, goods=Goods};
        "PEERS" ->
            decodeRouted(Content,
                fun
                    ([]) ->
                        #peers{};
                    (Remain) ->
                        throw({peersHadMoreContentThanARoute,Remain})
                end);
        "SREEP" ->
            decodeRouted(Content,fun (["#"|List]) -> #sreep{peers=List} end);
        "COST" ->
            decodeRouted(Content,fun (["#",Good]) ->  #cost{good=Good} end);
        "TSOC" -> 
            decodeRouted(Content,fun (["#",Good,"#",Sp,Sa,"#",Bp,Ba]) -> #tsoc{good=Good,buyPrice=Bp, buyAmount=Ba, sellPrice=Sp, sellAmount=Sa} end);
        _ ->
            throw({unkonwMessageType,Type})
    end.


decodeGoods(Content) ->
    [Src|["#"|List]] = Content,
    Goods = lists:map(
        fun (Element) -> 
                [Good,Ttl] = string:rokens(Element,"."),
                {Good,Ttl}
        end,List),
    {Src,Goods}.
                

decodeRouted(PElements,ContentHandler) ->
    {DoneRoute,["#"|Remain]} = lists:splitwith(fun(A) -> A /= "#" end,PElements),
    {TodoRoute,Content} = lists:splitwith(fun(A) -> A /= "#" end,Remain),
    #routed{
        routeDone = DoneRoute,
        routeTodo = TodoRoute,
        content = ContentHandler(Content)
    }.

validateName(Name) ->
    {ok,Compiled} = re:compile("^[0-9a-zA-Z_]*$"),
    case re:run(Name,Compiled) of
        {match,_} ->
            isValid;
        nomatch ->
            throw({invalidName,Name})
    end.

encodeMsg(#olleh{name=Name}) ->
    "OLLEH " ++ Name;
encodeMsg(#hello{name=Name}) ->
    "HELLO " ++ Name;
encodeMsg(#routed{routeTodo = Todo, routeDone = Done, content = Content}) ->
    RouteString = string:join(Done ++ ["#"] ++ Todo," "),
    case Content of
        #peers{} ->
            "PEERS " ++ RouteString;
        #sreep{peers=Peers} ->
            "SREEP " ++ RouteString ++ " # " ++ string:join(Peers," ");
        #cost{good=Good} ->
            "COST " ++ RouteString ++ " # " ++ Good;
        #tsoc{good=Good, buyPrice=Bp, buyAmount=Ba, sellPrice=Sp, sellAmount=Sa} ->
            "TSOC " ++ RouteString ++ " # " ++ Good ++ " # "++ Sp  ++ " " ++ Sa ++ " # " ++ Bp ++ " " ++ Ba
    end.

% --- Peersearch --------------------------------------------------------------

% found {name,[name,name,name]}
-record(peerState,{toTest,found,myName}).

peers() ->
    register(peered,self()),
    {neighs,Neighs} = gen_server:call(named,{getNeighs}),
    {myName,MyName} = gen_server:call(named,{getMyName}),
    Routes = lists:map(fun(X) -> [X] end,Neighs ),
    peers(#peerState{toTest=Routes,myName = MyName,found = [MyName|Neighs]}).


peers(State = #peerState{toTest=Routes,found=Found,myName=MyName}) ->
    case Routes of 
        []  -> 
            unregister(peered),
            {peers,Found};
        [Route|RestToTest] ->
            [NextHop|_OtherHops] = Route,
            sendMsg(NextHop,#routed{routeDone=[MyName], routeTodo=Route, content=#peers{}}),
            receive
                #routed{ routeDone = DoneRoute, content = #sreep{peers=Peers}} ->
                    NewPeers = lists:filter(
                        fun(NewPeer) -> 
                                not contains(NewPeer,Found)
                        end,Peers),
                    NewToTest = lists:map(
                        fun(NewCandidate) -> 
                                lists:reverse(DoneRoute) ++ [NewCandidate]
                        end,NewPeers),
                    peers(State#peerState{
                            toTest= RestToTest ++ NewToTest, 
                            found = Found ++ NewPeers})
            end
    end.
                                
contains(_Element,[]) ->
    false;
contains(Element,[Element|_T]) ->
    true;
contains(Element,[_H|T]) ->
    contains(Element,T).


% --- Logging -----------------------------------------------------------------

log(Obj) ->
    io:write(Obj).
log(Msg,Obj) ->
    io:write(Msg),
    io:write(Obj).
