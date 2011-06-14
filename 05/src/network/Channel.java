package network;


import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import java.util.StringTokenizer;

import messages.HelloMessage;
import messages.MessageType;
import messages.OllehMessage;
import messages.ProtocollMessage;
import primitives.Pair;

public class Channel{
	
	public static final int MAX_PACKET_LENGTH = 1440;
	
	DatagramSocket socket;

	
	public Channel(SocketAddress local) {
		try {
		    socket = new DatagramSocket(local);
		    
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
		MessageType type = null;
		try{
		    type = MessageType.valueOf(typeStr);
		} catch(IllegalArgumentException e) {
		    System.err.println("GARBAGE: " + data);
		    return null;
		}
		ProtocollMessage msg = null;
		
		switch (type) {
        case HELLO:
            msg = HelloMessage.parse(data);
            break;
        case OLLEH:
            msg = OllehMessage.parse(data);
            break;
        default:
            System.err.println("GOT GARBAGE: " + data);
            break;
        }
		

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
