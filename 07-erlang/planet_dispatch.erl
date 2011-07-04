-module(planet_dispatch).
-behaviour(gen_server).
-export([init/1,handle_call/3,code_change/3,handle_cast/2,handle_info/2,
        terminate/2]).

-include("./messages.hrl").
-record(state,{net,ets,sdoog,sreep,tsoc}).

init({Port,Name}) ->
    Net = gen_server:start(planet_net,{Port,Name},[]);
    Ets = ets:new(yeah,[set]),
    {ok,#state{ets=Ets,net=Net}}.

handle_call(_Msg,_Sender,State) ->
    {reply,ok,State}.

handle_cast(Msg, State=#state{net=Net, ets=Ets}) ->
     case Msg of
         #goods{} ->
             NewListner = startGoodsHandler(Net,Ets,Msg),
             {noreply,State#state{sdoog= NewListner}};
         #sdoog{} ->
             State#state.sdoog ! Msg;
         #routed{content=ContentMsg} ->
             case ContentMsg of
                 #peers{} ->
                     ok;
                 #sreep{} ->
                     State#state.sreep ! Msg;
                 #cost{} ->
                     ok;
                 #tsoc{} ->
                     State#state.tsoc ! Msg
         end
    end.


handle_info(_Info,State) ->
    {noreply,State}.

code_change(_OldVsn,State,_Extra) ->
        {ok,State}.

terminate(_Reason,_State) ->
    ty.

% ----------------------------------------------------------------------------

getUnixSeconds() ->
    {Mega,Sec,_} = erlang:now(),
    Mega * 1000 * 1000 + Sec.

startGoodsHandler(Net,Ets,Msg) ->
    spawn( fun() -> goodsHandler(Net,Ets,Msg) end).

goodsHandler(Net,Ets,#goods{src=Src,goods=SrcGoods}) ->
    Now = getUnixSeconds(),
    true = ets:insert(Ets,{{goods,Src},{Now,SrcGoods}}),
    AllGoods = checkGoods(Net,Ets),
    MyName = gen_server:call(Net,getMyName),
    gen_server:call(Net,{send,Src,#goods{src=MyName,goods=AllGoods}}).

checkGoods(Net,Ets) ->
    Now = getUnixSeconds(),
    Neighs = gen_server:call(Net,getAllNeighs),
    %% get all nodes that shall be chcked
    ToCheck = lists:filter(
        fun
            (Neigh) ->
                Key = {goods,Neigh},
                case ets:lookup(Ets,Key) of
                    [] ->
                        true = ets:insert(Ets,{Key,{Now,[]}});
                    [Key,{Time,Goods}] ->
                        if
                            Time + 30 < Now ->
                                true = ets:insert(Ets,{Key,{Now,Goods}});
                            true ->
                                false
                        end
                end
        end,Neighs),
    [{myGoods,MyGoods}] = ets:lookup(Ets,myGoods),
    sendGoodsRequest(Net,ToCheck,MyGoods),
    collectResponse(ToCheck,Ets),
    AllGoods = ets:foldr(
        fun
            ({{goods,_Neigh},{_Time,Goods}},Acc) ->
                Acc ++ Goods;
            (_,Acc) ->
                Acc
        end,
        [],
        Ets) ++ MyGoods,
    ets:insert(Ets,{myGoods,AllGoods}),
    AllGoods.

sendGoodsRequest(Net,ToCheck,MyGoods) ->
    MyName = gen_server:call(Net,getMyName),
    Msg = #goods{src=MyName,goods=MyGoods},
    lists:forEach(
        fun
            (Planet) ->
                gen_server:call(Net,{send,Planet,Msg})
        end,ToCheck).

collectResponse(ToCheck,Ets) ->
    Now = getUnixSeconds(),
    lists:forEach(
        fun
            (Neigh) ->
                Key = {goods,Neigh},
                receive
                    #sdoog{src=Neigh,goods=Goods} ->
                        true = ets:insert(Ets,{Key,{Now,Goods}})
                after
                    2000 ->
                        skip
                end
        end,ToCheck),
    ok.
% ---------------------------------------------------------------------------


