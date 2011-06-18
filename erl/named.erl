-module(named).
-behaviour(gen_server).
-export([init/1,handle_call/3,code_change/3,handle_cast/2,handle_info/2,terminate/2]).

% ------------- name table -------------------

% neig = {name,{ip,port}}
-record(namedState,{neigh,myName}).

init(MyName) ->
    {ok,#namedState{myName = MyName}}.

handle_call(Msg,_Remote,State = #namedState{neigh = Table,myName = MyName}) ->  
        case Msg of 
            {getName,Addr} ->  % Addr = {Ip,Port}
                {Name,Addr} = lists:keyfind(Addr,2,Table),
                {reply,{name,Name},State};
            {getAddr,Name} ->
                {Name,Addr} = lists:keyfind(Name,1,Table),
                {reply,{addr,Addr},State};          
            {addEntry,Name,Addr} ->
                {reply,ok,State#namedState{neigh = [{Name,Addr}|Table]}};
            {getMyName} ->
                {reply,{myName,MyName},State};
            {stop} ->
                {stop,shutdown,State}  
        end.

code_change(_OldVsn,State,_Extra) ->
    {ok,State}.

handle_cast(_Request, State) ->
    {noreply,State}.

handle_info(_Info, State) ->
    {noreply,State}.

terminate(_Reason,_State) ->
    ok.
