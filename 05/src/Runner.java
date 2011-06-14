import java.net.Inet4Address;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.UnknownHostException;

import applications.Cli;

import network.Channel;
import network.NamedChannel;


public class Runner {
	
	
	public static void main(String args[]){
		
		if(args.length != 2){
			System.err.println("Usage: vs05.jar <name> <localport>");
			return;
		}
		
		String nodeName = args [0];
		int port = Integer.parseInt(args[1]);
		
		
	    SocketAddress saddr = new InetSocketAddress(port);
		Channel chan = new Channel(saddr);
		NamedChannel n = new NamedChannel(nodeName,chan);
		Cli c = new Cli(n);
		
		new Thread(n).start();
		
		
		while(true){
			c.waitForCommand();
		}
		
	}

}
