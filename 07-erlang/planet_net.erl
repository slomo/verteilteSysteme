-module(planet_net).
-behaviour(gen_server).

-include("./messages.hrl").

-export([start/4]).
-export([init/1,handle_call/3,code_change/3,handle_cast/2,handle_info/2,terminate/2]).
-define(UDP_PACKET_LENGTH,200).

-record(state,{socket,myName,nameTable,dispatcher}).


% ---- Behaviour interface ----------------------------------------------------------------


start(Ip,Port,Name,RecvHandler) ->
    gen_server:start(?MODULE,{Ip,Port,Name,RecvHandler},[]).

init({Port,Name}) ->
    % start named
    {ok,Socket} = gen_udp:open(Port),
    gen_udp:controlling_process(Socket,self()),
    {ok,#state{socket=Socket,myName=Name}}.

handle_call(Request,_Sender,State = #state{nameTable = Table, myName = MyName}) ->
    case Request of
        {send,Target,Message} ->
            startSender(Target,Message,State),
            {reply,ok,State};
        {connect,{Ip,Port}} ->
            startSender({Ip,Port},#hello{name=MyName},State),
            {reply,ok,State};
        getAllNeigh ->
            {reply,planet_nt:getAllNeighs(Table),State};
        getMyName ->
            {reply,MyName,State};
        {updateNeigh,Name,Addr} ->
            {reply,ok,State#state{ nameTable = planet_nt:putNameAddr(Name,Addr,Table)}}
    end.

handle_cast(Request, State) ->
    log(handel_castUnkonwMEssage,Request),
    {noreply,State}.

handle_info(Info, State) ->
    case Info of
        {udp, _Socket, IP, InPortNo, Packet} ->
            spawn(fun() -> startReceiver({IP,InPortNo},Packet,State,erlang:self()) end),
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

startSender(Target,Message,#state{socket=Socket,nameTable=Table}) ->
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
startReceiver(Source,String,#state{nameTable=Table,myName=MyName,dispatcher=Dispatcher},Netd) ->
    try planet_proto:decode(String) of
        #hello{name = Name} ->
            gen_server:call(Netd,{updateNeigh,Name,Source}),
            gen_server:call(Netd,{send,Source,#olleh{name = MyName}});
        #olleh{name = Name} ->
            gen_server:call(Netd,{updateNeigh,Name,Source});
        RoutedMessage = #routed{} ->
            case routeMsg(RoutedMessage,MyName) of
                 #routed{content=Content} ->
                    case Content of
                        #peers{} ->
                            {NextHop,NextMessage} = answerPeers(RoutedMessage,Table),
                            gen_server:call(Netd,{send,NextHop,NextMessage});
                        _ -> 
                            dispatchMsg(RoutedMessage,Dispatcher)
                    end;
                {NextHop,NextMessage} ->
                    gen_server:call(Netd,{send,NextHop,NextMessage})
            end;
        OtherMessage -> 
            dispatchMsg(OtherMessage,Dispatcher)
    catch
        _SomeFidilingException -> throw(unableToProcessMessage)
    end.

answerPeers(#routed{routeDone=Done,routeTodo=Todo,content=#peers{}},Table) ->
    Neighs = planet_nt:getAllNeighs(Table),
    NewRoute = lists:reverse(Done),
    [NextHop|_] = NewRoute,
    {NextHop,#routed{routeDone=Todo,routeTodo=NewRoute,content=#sreep{peers=Neighs}}}.

dispatchMsg(Msg,Dispatcher) ->
    gen_server:cast(Dispatcher,Msg).

routeMsg(Msg = #routed{routeDone=Done,routeTodo=Todo,content=Content},MyName) ->
    case Todo of
        [MyName] ->
            Msg;
        [MyName|Remain] ->
            NextMsg = #routed{routeTodo=Remain,routeDone = Done ++ [MyName],content=Content},
            [NextHop|_] = Remain,
            {NextHop,NextMsg};
        _OtherRoutes -> throw({invalidRoutedMessage,Msg})
    end.

log(A,B) ->
    error_logger:info_report({A,B}).
