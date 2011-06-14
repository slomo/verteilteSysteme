package applications;

import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.Set;
import java.util.StringTokenizer;

import com.sun.xml.internal.fastinfoset.algorithm.IntEncodingAlgorithm;

import network.NamedChannel;

public class Cli {
	
	NamedChannel chan;
	
	public Cli(NamedChannel c){
		chan = c;
	}
	
	public void waitForCommand(){

		try {
			String eingabe =  new jline.ConsoleReader().readLine();
			
			StringTokenizer t = new StringTokenizer(eingabe);
			
			String first = t.nextToken();
			
			if(first == "connect") {
				doConnect(t.nextToken(),Integer.parseInt(t.nextToken()));
			} else {
				doPeerSearch();
			}
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	public void doPeerSearch(){
		PeerSearch p = new PeerSearch(chan
				);
		Set<String> s = p.doPeerSearch();
		
		
		System.out.println("Peers");
		for(String peer : s){
			System.out.println(peer);
		}
	}
	
	public void doConnect(String host, int port){
		chan.connect(new InetSocketAddress(host, port));
		System.out.println("Connetion might be established!");
	}

}
