import java.net.InetSocketAddress;

import network.Channel;
import network.NamedChannel;


public class Runner {
	
	
	public static void main(String args[]){
		
		
		Channel chan = new Channel(new InetSocketAddress(8081));
		
		NamedChannel n = new NamedChannel(chan);

		n.run();	
		
	}

}
