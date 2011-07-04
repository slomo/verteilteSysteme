-module(planet_proto).
-include("./messages.hrl").
-export([decode/1,encode/1]).

decode(A) ->
    decodeMsg(A).

encode(A) ->
    encodeMsg(A).

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
        "BYE" ->
            [Name|[_Garbage]] = Content,
            validateName(Name),
            #bye{name=Name};
        "EYB" ->
            [Name|[_Garbage]] = Content,
            validateName(Name),
            #eyb{name=Name};
        "BUY" ->
            [Good,Amount,Price] = Content,
            #buy{good=Good,
                amount=list_to_integer(Amount),
                price=list_to_integer(Price)
            };
        "YUB" ->
            [Good,Amount,Price] = Content,
            #yub{good=Good,
                amount=list_to_integer(Amount),
                price=list_to_integer(Price)};
        "SELL" ->
            [Good,Amount,Price] = Content,
            #sell{good=Good,
                amount=list_to_integer(Amount),
                price=list_to_integer(Price)};
        "LLES" ->
            [Good,Amount,Price] = Content,
            #lles{good=Good,
                amount=list_to_integer(Amount),
                price=list_to_integer(Price)
            };
        "WHOIS" ->
            [Planet] = Content,
            #whois{planet=Planet};
        "SIOHW" ->
            [Planet,SIp,SPort] = Content,
            Port = list_to_integer(SPort),
            [I1,I2,I3,I4] = string:tokens(SIp,"."),
            IP = {
                list_to_integer(I1),
                list_to_integer(I2),
                list_to_integer(I3),
                list_to_integer(I4)
            },
            #siohw{planet=Planet, addr={IP,Port}};
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

encodeMsg(#goods{src=Src, goods=Goods}) ->
    string:join("GOODS"++Src++"#"++Goods);
encodeMsg(#sdoog{src=Src, goods=Goods}) ->
    string:join("SDOOG"++Src++"#"++Goods);
encodeMsg(#olleh{name=Name}) ->
    "OLLEH " ++ Name;
encodeMsg(#hello{name=Name}) ->
    "HELLO " ++ Name;
encodeMsg(#bye{name=Name}) ->
    "BYE " ++ Name;
encodeMsg(#eyb{name=Name}) ->
    "EYP " ++ Name;
encodeMsg(#whois{planet=Planet}) ->
    "WHOIS " ++ Planet;
encodeMsg(#siohw{planet=Planet,addr={{I1,I2,I3,I4},Port}}) ->
    "WHOIS " ++ Planet ++ " " ++
    "." ++ integer_to_list(I1) ++
    "." ++ integer_to_list(I2) ++
    "." ++ integer_to_list(I3) ++
    "." ++ integer_to_list(I4) ++
    " " ++ integer_to_list(Port);
encodeMsg(#sell{good=Good, amount=Amount, price=Cost}) ->
    "SELL " ++ Good ++ " " ++ integer_to_list(Amount)
    ++ " " ++ integer_to_list(Cost);
encodeMsg(#lles{good=Good, amount=Amount, price=Cost}) ->
    "LLES " ++ Good ++ " " ++ integer_to_list(Amount) ++
    " " ++ integer_to_list(Cost);
encodeMsg(#buy{good=Good, amount=Amount, price=Cost}) ->
    "BUY " ++ Good ++ " " ++ integer_to_list(Amount) ++ " " ++ integer_to_list(Cost);
encodeMsg(#yub{good=Good, amount=Amount, price=Cost}) ->
    "YUB " ++ Good ++ " " ++ integer_to_list(Amount) ++ " " ++ integer_to_list(Cost);
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

