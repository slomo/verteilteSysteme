-module(netd).
-behaviour(gen_server).

-include("./messages.hrl").

-export([start/4]).
-export([init/1,handle_call/3,code_change/3,handle_cast/2,handle_info/2,terminate/2]).
-define(UDP_PACKET_LENGTH,200).

-record(addr,{port,ip}).
-record(state,{socket,myName,nameTable,distpatcher}).


% ---- Behaviour interface ----------------------------------------------------------------


start(Ip,Port,Name,RecvHandler) ->
    gen_server:start(?MODULE,{Ip,Port,Name,RecvHandler},[]).

init({Port,Name}) ->
    % start named
    {ok,Named} = gen_server:start(named,Name,[]),
    {ok,Socket} = gen_udp:open(Port),
    gen_udp:controlling_process(Socket,self()),
    {ok,#state{socket=Socket,named=Named}}.

handle_call(Request,_Sender,State = #state{socket = Socket, nameTable = Table, myName = MyName}) ->
    case Request of
        {send,{Ip,Port},Message} ->
            startSender(Target,Message,State);
            {reply,ok,State};
        {connect,{Ip,Port}} ->
            startSender({Ip,Port},#hello{name=MyName}),
            {reply,ok,State};
        getAllNeigh ->
            {reply,planet_nt:getAllNeighs(Table),State};
        getMyName ->
            {reply,MyName,State}
        {updateNeigh,Name,Addr} ->
            {reply,ok,State#state{ nameTable = planet_nt:putNameAddr(Name,Addr,Table)}}
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
    gen_udp:close(Socket).


% --- sender process

startSender(Target,Message,State{socket=Socket,nameTable=Table}) ->
    erlang:spawn(
        fun
            () ->
                String = planet_proto:encode(Message),
                case Target of
                    {Ip,Port} ->
                        gen_udp:send(Socket,Ip,Port,String);
                    Name ->
                        {Ip,Port} = planet_nt:getAddr(Name,Table),
                        gen_udp:send(Socket,Ip,Port,String)
                end
        end).

% --- receiver process

startReceiver(_Source,String,State) ->
    ok.

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
                #goods{} ->
                    warehouse ! Msg;
                #sdoog{} ->
                    warehouse ! Msg;
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
                    peered ! Msg;
                #cost{} ->
                    warehouse ! Msg;
                #tsoc{} ->
                    warehouse ! Msg
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

