package network;

import java.io.IOException;
import java.net.SocketAddress;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import messages.HelloMessage;
import messages.MessageType;
import messages.OllehMessage;
import messages.ProtocollMessage;
import primitives.Pair;

public class NamedChannel implements Runnable {

	String myName = "DEFAULT";

	BlockingQueue<Pair<String, ProtocollMessage>> inbox = new LinkedBlockingQueue<Pair<String, ProtocollMessage>>();

	Map<String, SocketAddress> addresses = new HashMap<String, SocketAddress>();
	Map<SocketAddress, String> names = new HashMap<SocketAddress, String>();

	Channel chan;

	public NamedChannel(Channel chan){
		this.chan = chan;
	}
	
	public Set<String> getNeigh(){
		return addresses.keySet();
	}
	
	public void send(String name, ProtocollMessage msg) {
		try {
			chan.send(addresses.get(name), msg);
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public Pair<String, ProtocollMessage> recv() throws InterruptedException {
		return inbox.take();
	}

	protected void handleNames(Pair<SocketAddress, ProtocollMessage> in)
			throws IOException {
		HelloMessage msg;
		if (in.second instanceof HelloMessage) {
			msg = (HelloMessage) in.second;
			chan.send(in.first, new OllehMessage(myName));
		} else if (in.second instanceof OllehMessage) {
			msg = (OllehMessage) in.second;
		} else {
			return;
		}

		names.put(in.first, msg.name);
		addresses.put(msg.name, in.first);
	}

	@Override
	public void run() {
		try {
			for (;;) {
				try {
				    System.err.println("WAITING FOR MESSAGE");
					Pair<SocketAddress, ProtocollMessage> in = chan.recv();
					
					if(in == null){
					    continue;
					}
					
					System.err.println("MESAGE RECEIVED");
					MessageType type = in.second.getType();
					if (type == MessageType.HELLO || type == MessageType.OLLEH) {
						handleNames(in);
					} else {
						inbox.put(new Pair<String, ProtocollMessage>(names.get(in.first), in.second));
					}
				} catch (IOException e) {
					// TODO Auto-generated catch block
					e.printStackTrace();
				}
			}
		} catch (InterruptedException e) {
			// TODO shutdown
		}
	}
}
