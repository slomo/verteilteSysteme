package network;

import java.io.IOException;
import java.net.SocketAddress;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import primitives.Pair;
import messages.ProtocollMessage;

public class NamedChannel implements Runnable{
	
	BlockingQueue<Pair<String,ProtocollMessage>> inbox = new LinkedBlockingQueue<Pair<String,ProtocollMessage>>();
	
	Map<String,SocketAddress> addresses;
	
	Channel chan;
	
	@Override
	public void run() {
		// TODO Auto-generated method stub
		
	}
	
	
	public void send(String name, ProtocollMessage msg) {
		
		try {
			chan.send(addresses.get(name), msg);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
	}
	
	public Pair<String,ProtocollMessage>recv(){
		try {
			return inbox.take();
		} catch (InterruptedException e) {
			e.printStackTrace();
			return null;
		}
	}

	

}
