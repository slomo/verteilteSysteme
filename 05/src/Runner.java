import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.UnknownHostException;

import network.Channel;
import network.NamedChannel;


public class Runner {
	
	
	public static void main(String args[]){
		
		
	    SocketAddress saddr = null;
        try {
            saddr = new InetSocketAddress(InetAddress.getByName("localhost"), 8081);
        } catch (UnknownHostException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
	    
		Channel chan = new Channel(saddr);
		
		NamedChannel n = new NamedChannel(chan);

		n.run();	
		
	}

}
