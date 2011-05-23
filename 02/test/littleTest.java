import static org.junit.Assert.*;

import java.net.InetSocketAddress;

import org.junit.Test;

import udp.UdpChannel;
import udp.UdpChannelFactory;
import udp.models.UdpMessage;
import vsFramework.Message;



public class littleTest {
    
    @Test
    public void testSmall(){
        
        byte[] content = "Some content".getBytes();
        Message m = new UdpMessage(content);
        
        InetSocketAddress localhost = new InetSocketAddress("localhost",1700);
        int recvPort = 3000;
        int sendPort = 4000;
        
        UdpChannel recv = UdpChannelFactory.newUdpChannel(recvPort);
        UdpChannel send = UdpChannelFactory.newUdpChannel(sendPort,localhost,recvPort);
        
        
        send.send(m);
        
        m = recv.recv();
        
        assertEquals(m.getData(),content);
        
    }

}
