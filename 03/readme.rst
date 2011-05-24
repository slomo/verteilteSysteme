----------------------------------
Verteilte Systeme Aufgabenblatt 3
----------------------------------

Georg Graf und Yves Müller


non-blocking revc
-----------------

Da wir letzte mal aus Krankheitsgründen keine Implmentierung hatten, benutzen
wir die Musterlösung als Basis. Diese implmentiert schon nrecv durch den Aufruf
von peek() auf die BlockingQueue mit den eingehenden Nachrichten.


Synchrone Prozesse
------------------

Zur Simulation eines Synchronenalgoriothmus, verwenden wir in Threads laufende
Konten (implementiert in der Klasse abstract node). Und eine Prozessbariere
implementiert über einen Kontrollprozess.

Die Knoten liegen auf einem Ring, Kommunikation ist in der implementierten
Version nur in eine Richtung des Ring möglichs (Erweiterungen sind denkbar). 

Das Node-Interface
..................

Das *Node* Interface bietet 3 Methoden. Zum einen erbt der Node von Runnabel, so
dass man ihn mit der run() Methode starten kann. Außerdem hat eine Methode
nextRound, diese soll später ersetzt werden durch die Implmentierung einer Runde
des synchronen Algorithmus. Beim start einer Runde erhält der Prozess die
Rundennummer sowie die Nachricht seines Vorgängers. Die Rückgabe des Knoten wird
als Nachricht an den nächsten Nachbarn in der nächsten Runde versendet.

Implementiert wird das Interface durch die abstrakte Klasse *AbstractNode*, sie
enthält jegliche Logik zur Implementierung der Runden und erfüllt auch schon das
Runnable-Interface. Dazu wartet der Prozess nach start auf eine Nachricht des
Kontrollprozess, der ihm mitteilt, dass er das die Nachricht des Vorgängerslesen
kann. Anschließend wird durch Aufruf der Methode *nextRound* eine neuen
Nachricht erzeugt und diese versendet, nach dem der Kontrollprozess dies erlaubt
hat.

Der Kontrollprozess
-------------------

Durch den Kontrollprozess wird eine zweistufige Barriere implementiert. In Stufe 1 
wird von den Prozessen eine Nachricht empfangen und bearbeitet, in Stufe zwei wird
eine Antwort gesendet.

::
    
    public void performRound(byte number){
        Message m = new ByteMessage(number);
        // tell nodes that round started
        for( Channel n : nodes){
            n.send(m);
        }
        // wait for computation of new message
        for( Channel c : nodes){
            Message n = c.recv();
            assert(n.getData()[0] == number);
        }
        // tell nodes to write output
        for( Channel n : nodes){
            n.send(m);
        }
        // wait for till all nodes are done
        for( Channel c : nodes){
            Message n = c.recv();
            assert(n.getData()[0] == number);
        }
    }


Retrosperktive des Frameworks
.............................

Das Framework vereinfacht zum einen das Problem deutlich, in dem ein Ring 
angenommen wird. Ein weiter Nacheil ist, das wir es  nicht geschafft hab ein
ordentliches Anhalten der Nodes zu implmentieren. Da die Nodes in einer
Endlosschleife laufen, und in dieser meist auf den Knontrollkanal lauschen.

Die Vermengung von Frameworklogik und eigentlicher Logik in Node, bzw dessen 
Implmentierungen ist ebenfalls eine Schwäche des Frameworks. Sie drückt sich
darin aus, das nur Nodes die von AbstractNode erbene ohne weiteres korrekt 
funktionieren. Allerdings war die Lösung sehr einfach.


TimeSlice
----------

Die TimeSlice Implmentierung findet sich im TimeSliceNode. Über TimeSliceRun 
wird sie gestartet, dort lässt sich auch mittels dem Array ids, die Prozessid 
und Reihenfolge der einzelnen Knoten einstellen.

Die Implmentierung selbst hält den Status in zwei bools. Zum einen wird geguckt, 
ob der Prozess sich selber zur Wahl stellt, oder zum anderne schon einen anderen 
Prozess (verläufig) akzeptiert hat. 

Empfängt er kein Paket, stellt er sich in der Runde mit seiner ID zur Wahl, es sei den er akzeptiert vorher das Announcment eines anderen Prozesses. Wird ein Paket empfangen, so wird anhand der ID im Pakete fallunterschiedeb. Ist sie größer als meine eigene verwerfe ich das Pakete, es kann nicht gewinnen. Ist es genauso groß wie meine dann habe ich mein Annoucment wieder erhalten und gewonnen. Ist es kleiner, gebe ich auf und akzeptiere den Prozess.

Dies wird fast 1:1 im Code abgebildet:

::

     // got message
        if (mesg != null) {
            byte content = mesg.getData()[0];
            if (chooseMyself) {
                if (content < id) {
                    System.out.println("Round[" + round + "]:" + "Giving up in favor of " + content + " says " + id);
                }
                if (content > id) {
                    System.out.println("Round[" + round + "]:" + "To big, dumping " + content + " says " + id);
                    return null;
                }
                if (content == id) {
                    System.out.println("Round[" + round + "]:" + "Leader is now me says " + id);
                    return null;
                }
            } else {
                System.out.println("Round[" + round + "]:" + "Forwarded " + content + " says " + id);
                acceptedOther = true;
            }
            return mesg;
        } else {
            // it is my turn
            if (round == id && !acceptedOther) {
                System.out.println("Round[" + round + "]:" + "Trying to be leader says " + id);
                chooseMyself = true;
                return new ByteMessage(id);
            } else {
                return null;
            }
        }

