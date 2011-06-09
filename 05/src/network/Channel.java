package network;


import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.net.SocketException;
import java.util.StringTokenizer;

import messages.MessageType;
import messages.ProtocollMessage;
import primitives.Pair;

public class Channel{
	
	public static final int MAX_PACKET_LENGTH = 1440;
	
	DatagramSocket socket;

	
	public Channel(SocketAddress local) {
		try {
			socket = new DatagramSocket();
			socket.bind(local);
		} catch (SocketException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}
	
	public Pair<SocketAddress,ProtocollMessage> recv() throws IOException{
		
		byte buffer[] = new byte[MAX_PACKET_LENGTH]; 
		DatagramPacket p = new DatagramPacket(buffer, buffer.length);
		socket.receive(p);
		
		
		String data = new String(p.getData());
		StringTokenizer token = new StringTokenizer(data," ");
		
		String typeStr = token.nextToken();
		
		MessageType type = MessageType.valueOf(typeStr);
		
		ProtocollMessage msg = null;
		try {
			msg = (ProtocollMessage) type.impl.newInstance();
		} catch (InstantiationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		msg.parse(data);

		return new Pair<SocketAddress,ProtocollMessage>(p.getSocketAddress(),msg);
	}
	
	// TODO: better exception
	public void send(SocketAddress addr,ProtocollMessage msg) throws IOException{
		
		byte data[] = msg.getData();
		DatagramPacket p = new DatagramPacket(data, data.length);
		p.setSocketAddress(addr);
		socket.send(p);		
	}
}
