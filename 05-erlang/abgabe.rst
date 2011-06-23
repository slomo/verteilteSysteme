=========================
Verteilte System Abgabe 5
=========================

von Yves Müller


Diesen Arbeitsblatt habe ich nach vergeblichen Versuchen in Java in
Erlang entwickelt. Die erste Frage die sich bei einer solchen Implmentierung
stellen muss, ist in welche Erlangprozesse das Problem zerlegt werden kann.

Prozesse
--------

::
    
      Socket
        ^
        ¦ .................................................
        ¦ : ......................                        :
        ¦ : :                    :                        :
        v : :                    V                        V
      _________            _________                  ___________
     |         |          | Handler |_               |           |
     | Socket  | -------> |_________| |_   --------> | Directory |
     | (inetd) | <-------  |__________| |  <-------- |  (named)  |
     |_________|             |__________|            |___________|
          ^                      ¦
          ¦                      ¦
          ¦                      V
          ¦                ______________
          ¦               |              |
           ---------------| Application  |
                          | (z.B. Peers) |
                          |______________|
        --- Messages
        ... Supervision


Der Prozess, der den UDP-Socket besitzt, nimmt Nachrichten entgegen und starte
für Abhandlung jeder Nachricht einen Handlerprozess. Dies ist möglich, da
Prozesse in Erlang deutlich leichtgewichtiger sind als zum Beispiel Threads in
Java.

Der Handler kümmert sich das um das decodieren und dispatchen der Nachricht.
Außerdem löst er den Abesender der eingehenden Nachricht auf. Wozu er die
Namensdatenbank, die ebenfalls ein einger Prozess ist anfragt. Die Datenbank
hält eine Zuordnung von IPs,Ports und Plantennamen und muss daher auch in einem
seperaten Prozess laufen. Muss ein Paket geroutet werden kümmern sich ebenfalls
der Handlerprozess darum, nur Pakete die für diesen Planten bestimmt sind,
werden direkt an die Applikation weiter geleitet, die ebenfalls in einem
eingenen Prozess läuft.

Der Erlangpholosophie folgend (let-it-crash) findet in keinem der Prozesse eine
Fehlerhandlung statt, außer im Sockethandler, da dieser als Supervisor der
anderen Prozesse fungiert. Dies hält den Code erfreulich schmal.

Behaviours
----------

Die Erlang/OTP Biblothek bietet vorgefertige Verhaltensweise für Prozesse an,
damit nicht immer wieder die gleichen Muster implmentiert werden müssen. Für den
Datenbankprozess und den Socketbesitzer nutze ich solche *behaviours*, und zwar
die *gen_server* behaviour. Sie reduziert den Aufwand bei der Implmentierung
eines Servers erheblich, denn es ist nur noch notwendig die Handler des Severs
zum implmentieren. Diese Vorgehensweised entspricht dem Erlangparadigma der
Trennung von sequentielle und nicht sequentiellem Code. Die restlichen Prozesse
habe ich aus Zeit gründen unsauberer in einer Artmischform implmentiert.

Commandline
-----------

Zur Zeit verfügt das Modul noch nicht über eine echte CLI, diese werde ich erst
zum nächsten Zettel nachrüsten. Nach dem Start der Erlanginstanz steht jedoch
die Erlang REPL zur Verfügung, mit der man über das Modul planet die Befehle
planet:start(port,name), planet:connect(ip,port) und planet:peers() absetzen kann.

::
    1> planet:start(8081,"n2").
    {ok,<0.37.0>}
    2> planet:connect({127,0,0,1},8082).
    ok
    3> planet:startPeers().
    {peers,["n2","n3","n1"]}

Implmentierung im Detail
------------------------

Nach diesem kurzen Überblick über die Architektur und die Funktionalität, nun
einige Kernpunkte meiner Implmentierung.

Zu erst einmal habe ich alle Messagearten als sogenannte Records (benannte
Tuple) implementiert.

::
    
    -record(hello,{name}).
    -record(olleh,{name}).
    -record(routed,{routeDone=[],routeTodo=[],content}).
    % contents of routed package
    -record(sreep,{peers}).
    -record(peers,{}).

Die Defenitionen an sich sind relativ unspektakulär. Die geroutete Nachrichten
enthalten zwei Routen, einmal die die schon gegangen wurde, und die restelichen
Knoten. So ist es einfacher zu ermitteln ob die Nachricht am Ziel ist, oder zu
wem sie weiter geleitet werden soll. Diese Logik findet sich in der *routeMsg*
Funktion.

::
    
    routeMsg(_Sender,Msg =
        #routed{routeDone=Done,routeTodo=Todo,content=Content}) ->
        {myName,MyName} = gen_server:call(named,{getMyName}),
        case Todo of
            [MyName] -> % dispatch
                case Content of
                    _ -> peered ! Content
                end;
        [MyName|Remain] ->
            NextMsg = #routed{
                routeTodo=Remain,
                routeDone=Done++[MyName],
                content=Content
            },
            [NextHop|_] =Remain,
            gen_server:call(netd,{send,NextHop,NextMsg});
        _ ->
            throw({invalidRoutedMessage,Msg})
    end.

Falls nur noch ein Knoten in der weiteren Liste drin ist, wird die Nachricht
weitergeleitet. Andernfalls wird die Nachricht an peered, dir zur Zeit einzige
Applikation weitergeleitet.

Ausblick
--------

Zur nächsten Abgabe versuch ich eine vernüftige cli dazu zu packen, und die
Usabillty zu erhöhen. Außerdem gebe ich pünktlich ab, und alles wird besser ;)
