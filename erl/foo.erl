-module(foo).
-behaviour(gen_server).

-export([start/2]).
-export([init/1,handle_call/3,code_change/3,handle_cast/2,handle_info/2,terminate/2]).
-define(UDP_PACKET_LENGTH,200).

% TODO: DEBUG ONLY
-export([decodeMsg/1,encodeMsg/1]).

% --- Messages ----------------------------------------------------------------
-record(hello,{name}).
-record(olleh,{name}).

-record(routed,{routeDone=[],routeTodo=[],content}).
% contents of routed package
-record(sreep,{peers}).
-record(peers,{}).
% -----------------------------------------------------------------------------

-record(state,{socket,named}).

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


sendMsg(Msg) ->
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
                _ -> todo
            end;
        [MyName|Remain] ->
            NextMsg = #routed{routeTodo=Remain,routeDone = Done ++ [MyName],content=Content},
            [NextHop|_] = Remain,
            gen_server:call(netd,{send,NextHop,NextMsg});
        _ -> throw({invalidRoutedMessage,Msg})
    end.

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
        "PEERS" ->
            decodeRouted(Content,
                fun
                    ([]) ->
                        #peers{};
                    (Remain) ->
                        throw({peersHadMoreContentThanARoute,Remain})
                end);
        "SREEP" ->
            decodeRouted(Content,
                fun
                    (["#"|List]) ->
                        #sreep{peers=List}
                end);
        _ ->
            throw({unkonwMessageType,Type})
    end.

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
            "SREEP " ++ RouteString ++ " # " ++ string:join(Peers," ")
    end.

% --- Peersearch --------------------------------------------------------------

% found {name,[name,name,name]}
-record(peerState,{toTest,found,myName}).

start() ->
    {neighs,Neighs} = gen_sever:call(named,{getNeighs}),
    {myName,MyName} = gen_server:call(named,{getMyName}),
    peers(#state{toTest=Neighs,myName = MyName}).


peers(State = #state{toTest,found}) ->
    receive
        #sreep{doneRoute = DoneRoute, peers=Peers} ->
            


% --- Logging -----------------------------------------------------------------

log(Obj) ->
    io:write(Obj).
log(Msg,Obj) ->
    io:write(Msg),
    io:write(Obj).
