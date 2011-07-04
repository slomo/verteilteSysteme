-module(planet_dispatch).
-behaviour(gen_server).
-export([init/1,handle_call/3,code_change/3,handle_cast/2,handle_info/2,terminate/2]).


-include("./messages.hrl").
-record(state,{listner,planetPids,net}).
-record(listner,{sdoog,sreep,tsoc}).
-record(lager,{planetPids}).

handle_cast(Msg, State,State #state{listner=Listner,net=Net,lager=Lager) ->
     case Msg of
         #goods{} ->
            updatePlanetPids(PlanetPids,Net),
            startGoodHandler(Lager,Net).
            State#state{planetPids=PlanetPids}
         #sdoog{} ->
             State#state.sdoog ! Msg;
         #routed{content=ContentMsg} ->
             case ContentMsg of
                 #peers{} ->
                     State#state.peers ! Msg;
                 #sreep{} ->
                     State#state.sreep ! Msg;
                 #cost{} ->
                     State#state.cost ! Msg;
                 #tsoc{} ->
                     State#state.tsoc ! Msg
         end 
    end.

handle_info(Info,State) ->
    {noreply,State}.

code_change(_OldVsn,State,_Extra) ->
        {ok,State}.

terminate(_Reason,_State) ->
    ty.

% ----------------------------------------------------------------

updatePlanetPids(OldPids,Net) ->
    Neighs = gen_server:call(Net,{getAllNeighs}),
    lists:map(
        fun
            (Neigh) ->
                case lists:keyfind(Neigh,1,OldPids) ->
                    {Neigh,Pid} ->
                        {Neigh,Pid};
                    false ->
                        {Neigh,startPlanetWareProxy(Neigh,Net)}
                end
        end,OldPids).


startPlanetWareProxy(Net) ->


% --------------------------------------------------------------------

planetWareProxy(LastTime,Neigh,Net,RemoteGoods) ->
    {Mega,Secs,_Micro} = erlang:now(),
    Now = 1000 * 1000 * Mega + Sec,
    receive 
        {Src,getGoods,Goods} ->
            if
                Lastime + 30 < Now ->
                    MyName = gen_server:call(Net,getMyName),
                    gen_server:call(Net,{send,Neigh,#goods{src=MyName,goods=Goods}}),
                    receive 
                        #sdoog{src=Neigh,goods=NewRemoteGoods} ->
                            planetWareProxy(Now,



                

% ---------------------------------------------------------------------------

startWH() -> do(#wareState{ goods = [{"PetersMütze",20}], ages = []}).

% ages = dict
do(State= #wareState{goods=Lager,ages = Ages}) ->
    {myName,MyName} = gen_server:call(named,{getMyName}),
    receive 
        #goods{src = Src, goods = Goods} ->
            {_,Now,_} = now(),
            {neighs,Neighs} = gen_server:call(named,{getNeighs}),
            AdditionalAges = lists:map(fun(New) -> {New,0} end,
                lists:filter(fun(Neigh) -> lists:keyfind(Neigh,1,Ages) =:= false end, Neighs)
            ),
            UpdatedAges = lists:map(
                fun({Node,Age}) -> 
                        if
                            Age + 30 < Now -> 
                                sendMsg(Node,#goods{goods=Goods,src=MyName}),
                                {Node,Now};
                            true ->
                                {Node,Age}
                        end
                end,Ages ++ AdditionalAges),
            NewGoods =  lists:filter(
                fun({Good,Ttl}) -> (lists:keyfind(Good,1,Lager) =:= false) and (Ttl > 0) end, Goods),
            UpdatedGoods = Goods ++ lists:map(fun({Good,Ttl}) -> {Good,Ttl - 1} end,NewGoods),
            sendMsg(Src,#sdoog{goods=Goods,src=MyName}),
            do(#wareState{goods = UpdatedGoods, ages = UpdatedAges});
        #routed{ routeDone=Done, content = #cost{good = Good}} ->
            Todo = [NextHop|_] = lists:reverse(Done),
            sendMsg(NextHop,#routed{ routeTodo = Todo, routeDone = MyName, content = #tsoc{good=Good,buyPrice=100,buyAmount=100,sellPrice=100,sellAmount=100}})
    end.

