package applications;

import java.util.HashSet;
import java.util.Set;

import messages.PeersMessage;
import messages.ProtocollMessage;
import messages.SreepMessage;
import network.NamedChannel;

public class PeerSearch {

	public Set<String> peers = new HashSet<String>();
	public NamedChannel chan;
	public Routing r = new Routing();
	
	
	public PeerSearch(NamedChannel chan){
		this.chan = chan;
	}
	
	public Set<String> doPeerSearch(){
		
		// init routes
		r.updateRoutes(chan.myName, chan.getNeigh());
		
		Set<String> toCheck = new HashSet<String>();
		toCheck.addAll(chan.getNeigh());
		
		for(String node : toCheck){
			toCheck.remove(node);
			
			if(peers.contains(node)){
				continue;
			} else {
				ProtocollMessage msg = new PeersMessage(r.getRoute(node));
				chan.send(r.getRouteBase(node), msg);
				
				try {
					ProtocollMessage resp = chan.recv().second;
				} catch (InterruptedException e) {
					e.printStackTrace();
					return null;
				}
				
				SreepMessage s = (SreepMessage) msg;
				
				toCheck.addAll(s.getPeers());
			}
			
		}
		
		return peers;
	}
	
	
}
