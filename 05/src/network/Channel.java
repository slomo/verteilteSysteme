package network;


import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.SocketException;
import java.util.Map;

import primitives.Pair;

import messages.ProtocollMessage;

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
		
		
		byte[] data = p.getData();
		
		//TODO: dezerialze message
		
		ProtocollMessage msg = null;
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
