-module(foo).
-behaviour(gen_server).

-export([start/2]).
-export([init/1,handle_call/3,code_change/3,handle_cast/2,handle_info/2,terminate/2]).
-define(UDP_PACKET_LENGTH,200).

%% testing only
-export([decodeMsg/1,processPackage/3]).

% Messages 
-record(hello,{name}).
-record(olleh,{name}).

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

handle_call(Request,Sender,State = #state{socket = Socket}) ->
    case Request of     
        {udp, _Socket, IP, InPortNo, Packet} ->
            processPackage(IP, InPortNo,Packet),
            {noreply,State};
        {send,{Ip,Port},String} ->
            gen_udp:send(Socket,Ip,Port,String),
            {reply,ok,State};
        {_,send,Name,String} ->
            {reply,ok,State}
    end.

handle_cast(Request, State) ->
    log(handel_castUnkonwMEssage,Request),
    {noreply,State}.

handle_info(Info, State) ->
    case Info of     
        {udp, _Socket, IP, InPortNo, Packet} ->
            spawn(?MODULE,processPackage,[IP, InPortNo,Packet]),
            {noreply,State};
        _ ->
            log(handle_unkonwInfo,Info),
            {noreply,State}
    end.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.

terminate(_Reason,State = #state{socket= Socket}) ->
    unregister(netd),
    unregister(named),
    gen_udp:close(Socket),
    ok.



%% ------- processing logic


processPackage(IP, InPortNo, Packet) ->
    try decodeMsg(Packet) of
        #hello{name = Name} -> 
            gen_server:call(named,{addEntry,Name,{IP,InPortNo}}),
            {myName,MyName} = gen_server:call(named,{getMyName}),
            gen_server:call(netd,{send,{IP,InPortNo},encodeMsg(#olleh{name = MyName})});
        #olleh{name = Name} ->
            gen_server:call(named,{addEntry,Name,{IP,InPortNo}})
    catch
        _ -> throw(unableToParseMessage)
    end.

%% ------------- pack and unpack

% @param Packet raw input String
decodeMsg(Packet) -> 
    [Type | Content] = string:tokens(Packet," "),
    case Type of 
        "HELLO" -> 
            [Name|[]] = Content,
            #hello{name=Name};
        "OLLEH" -> 
            [Name|[]] = Content,
            #olleh{name=Name}
    end.

encodeMsg(#olleh{name=Name}) ->
    "OLLEH " ++ Name.

log(Obj) ->
    io:write(Obj).
log(Msg,Obj) -> 
    io:write(Msg),
    io:write(Obj).
