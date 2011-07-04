% --- Messages definitions ----------------------------------------------------
-record(hello,{name}).
-record(olleh,{name}).
-record(bye,{name}).
-record(eyb,{name}).

-record(whois,{planet}).
-record(siohw,{planet,addr}).

-record(goods,{src,goods}). % goods is a list of {name,ttl} Tupels
-record(sdoog,{src,goods}).

-record(buy,{good,amount,price}).
-record(yub,{good,amount,price}).
-record(sell,{good,amount,price}).
-record(lles,{good,amount,price}).


-record(routed,{routeDone=[],routeTodo=[],content}).
% contents of routed package
-record(sreep,{peers}).
-record(peers,{}).
-record(cost,{good}).
-record(tsoc,{good,buyPrice,buyAmount,sellPrice,sellAmount}).

