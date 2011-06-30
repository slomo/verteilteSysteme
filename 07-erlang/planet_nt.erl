-module(planet_nt).
-export([getAllNeighs/1,getName/2,getAddr/2,putNameAddr/3]).

% --- handle address table -----------------------------
%
% Table = {Name,{Port,Ip}}

getAllNeighs(Table) ->
    {Neighs,_Addrs} = lists:unzip(Table),
    Neighs.

getName(Addr,Table) ->
    {Name,Addr} = lists:keyfind(Addr,2,Table),
    Name.

getAddr(Name,Table) ->
    {Name,Addr} = lists:keyfind(Name,1,Table),
    Addr.

putNameAddr(Name,Addr,Table) ->
    _NewTable = lists:keystore(Addr,2,Table,{Name,Addr}).


