-module(planet).

-export([start/2]).
start(Port,Name) ->
    gen_server:start(planet_dispatch,{Port,Name},[]).
