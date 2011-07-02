import static org.junit.Assert.assertEquals;

import java.net.InetAddress;
import java.net.UnknownHostException;

import org.junit.Test;

import udp.UdpChannel;
import udp.UdpChannelFactory;
import udp.models.UdpMessage;
import vsFramework.Message;



public class littleTest {
    
    @Test
    public void testSmall() throws UnknownHostException{
        
        String str = "Some content";
        byte[] content = str.getBytes();
        Message m = new UdpMessage(content);
        
        InetAddress localhost = InetAddress.getByName("localhost");
        int recvPort = 3000;
        int sendPort = 4000;
        
        UdpChannel recv = UdpChannelFactory.newUdpChannel(recvPort);
        UdpChannel send = UdpChannelFactory.newUdpChannel(sendPort,localhost,recvPort);
        
        send.send(m);
        
        m = recv.recv();
        
        assertEquals(str,new String(m.getData()));
        
    }

}
