==========================
Verteilte System Abgabe 06
==========================

Yves Müller

Einleitung
----------

Wie auch bei der ersten Aufgabe habe ich das Programm in Erlang geschrieben.
Ziel dieser Iteration am Spiel war es, eine funktionierende Implmentierung von
goods und cost herzustellen. Für mich wurde dieser Versuch zu einem der
traurigsten Abschnitte in meiner Programmierkarierre und führt zu meiner
völligen Resignation in der Programmierung mit Erlang.

Anmerkungen zum Protokoll
-------------------------

Doch bevor zu den Problemen meiner Implmentierung komme, noch eine Anmerkung zum
Protokoll. Zum einen find ich das *Verb* Cost sehr verwirrend, das es neben den
Kosten auch Verfügbarkeit und Nachfrage einen Produk lieferts. Ein viel größeres
Problem stellt meiner Meinung nach aber die Nutzung eines Underscores als
Zeichen des nichts handeln mit einem Produkt dar. Die bricht völlig mit der
Struktur der Nachricht und führt in meiner Implmentierung (ich nehme keine
regulären Ausdrücke) so geschätzt 15 Zeilen mehr Code (Was in Erlang echt
verdammt viel ist). Daher habe ich mit der Kompatibilität gebrochen und einfach
0 0 für das nicht handeln eines Produktes verwendet. Man könnte vermuten das
jetzt etwas verschenkt wird auf dem Planeten, allerdings macht die Anzahl null
an Handelseinheiten meiner Meinung auch klar worum es geht.

Implementierung
---------------

In der letzten Abgabe hatte ich meine Prozessstruktur vorgestellt, diese habe
ich ein weiteres Mitglied erweitert, und zwar den *warehouse* Prozess, der für
die Warenverwaltung zuständig ist. Nach dem ich das ein- und auspacken von
Nachrichten implmentiert habe (ein Framework dafür wäre eigenlich echt genial),
stellte ich fest, dass ich den kompletten Routing code für cost Nachrichten mit
benutzen konnte. Durch die mehr oder weniger rein gefrikelte Peersearch, hatte
ich jedoch noch keine generische Kapslung der Breitensche, die auch weiterhin
fehlt. 

Ich entscheide mich erstmal das Goods-Problem anzugehen, da es mir einfacher
schien. Im prinzip handelt es sich dabei ja nur um einen Cache, wie ich ihn
schon öfter in Java implmentiert habe. Hier war der Fehler.

Dadurch, dass die Datenstrukturen in erlang ja immutable sind, wird das updaten
mehrer Cacheeinträge und eventuell mehrstufige Abgleichen mit anderen
Datensätzen (wie zum Beispiel den Nachbartabellen) zur Katastrophe. Nach meheren
Anläufen gelang es mir unter der Nutzung zahlreicher Listenfunktionen folgenden
Code zu erzeugen:

::
    
    do(State= #wareState{goods=Lager,ages = Ages}) ->
        receive 
            #goods{src = Src, goods = Goods} ->
                {_,Now,_} = now(),
                {myName,MyName} = gen_server:call(named,{getMyName}),
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
                do(#wareState{goods = UpdatedGoods, ages = UpdatedAges})
            %#cost{good = Good} ->
            %    sendMsg(#tsoc{good=Good,buyPrice=100,buyAmount=100,sellPrice=100,sellAmount=100})
        end.

Dieses Schnippsle leistet im Prinzip genau zwei Aufgaben, es hohlt alle Nachbarn
findet die neuen herraus und fragt, dann die Knoten an von denen keine aktuellen
Werte vorliegen.

An dieser stelle entsteht, dann auch eingewaltiges Konzeptionelle Problem meiner
Implementierung: Es gibt keine sinvolle Möglichkeit auf den Empfang aller
Goods-Nachrichten der Nachbarn zu warten. Dazu noch kurz einige Erläuterungen:
Es gibt einen zentralen Prozess in meiner Implmentierung der alle Pakte animmt
und diese dann von Workerprozessen parsen lässt. Anschließend dispatchen die
Workerprozess die Pakete, es sei denn sie müssen geroutete werden. Das
Dispatchen passiert, in dem die empfangenen Nachrichten zu
*Applicationprozesses* gesandt werden. Diese Prozesse werden gebraucht um den
Status der Anwendungslogik (Lagerhaus, Peersabfrage) zu halten, da Status in
Erlang nur in einem Prozess oder einer Nachricht existiert. Ich hab mich
entscheiden den Status in einem Prozess zu halten, damit ich ih nicht komplett
mit Nachrichten hin und her senden muss, zu mal dann der Status zwischen
verschiedne Nachrichten abgeglichen werden muss. Nach dem Empfangen der Goods
Nachricht fordere ich alle Nachbarn auf ihre Nachricht zu schicken. An dieser
Stelle müsste ich im Prinzip warten bis alle Antworten. Jedoch kann ich während
des aktiven Wartens keine andere Goods Nachrichten beantworten, was ich jedoch
im Falle eines Kreises tun muss. Warte ich nicht explizit, so erhalte ich alle
anderen Nachrichten nach und nach asynchron. Dann kann ich nicht entscheiden,
wann ich wieder an meinen ursprünglichen Anfrager zurücksenden kann. 

Fazit
=====

Nach einer Nacht durch programmieren, habe ich eine halbgare Lösung, die erst
bei der t+1 Anfrage alle Güter die mit Anfrage t hätten kommen müssen
beantworten. Zum wiederhohlten Male muss ich leider eine fehlerbhafte Lösung
abgeben. Die Cost-Nachrichten funktionieren soweit, leider gibt es noch keine
Simulation von Nachfrage. Ich fürchte, um die Anfrage korrekt umzusetzen und den
Code auch im Sinne des nächsten Zettels zu erweitern brauche ich eine andere
Architektur, oder zumindest umfangreiche Modifikationen. Dem werde ich mich im
Laufe dieser Woche zu wenden.


