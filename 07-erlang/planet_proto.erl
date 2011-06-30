-module(planet_proto).
-include("./messages.hrl").
-export(decode/1,encode/1).

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
            {Src,Goods} = decodeGoods(Content),
            #goods{src=Src, goods=Goods};
        "SDOOG" ->
            {Src,Goods} = decodeGoods(Content),
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
                [Good,Ttl] = string:tokens(Element,"."),
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

