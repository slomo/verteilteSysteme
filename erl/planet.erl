-module(planet).

-export([start/2]).
-define(UDP_PACKET_LENGTH,200).

start(Port,Name) ->
       case gen_udp:open(Port) of
              {ok, Socket} -> 
                     listen(Socket);
              {error, Reason} ->
                     log("Open socket failed: " ++ Reason)
       end.


listen(Socket) ->
       case gen_udp:recv(Socket,?UDP_PACKET_LENGTH) of
              {ok, Data} ->
                     log("Data: " ++ Data);
              {error, Reason} ->
                     log("Error recv data error: " ++ Reason)
       end.



log(Msg) -> 
        io:put_chars(standard_error,Msg).
