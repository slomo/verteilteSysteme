import java.net.InetSocketAddress;

import vsFramework.Channel;
import vsFramework.Message;


public class UdpChannel implements Channel {

    protected int localPort;
    
    public UdpChannel(int localPort){
        
        
        
    }
    
    @Override
    public void send(Message m) {
        // TODO Auto-generated method stub

    }

    @Override
    public Message recv() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void close() {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean hasBeenClosed() {
        // TODO Auto-generated method stub
        return false;
    }
    
    /**
     * @return the remote address we are connected to.
     */
    public InetSocketAddress getRemoteAddress() {
        // TODO Auto-generated method stub
        return null;
    }
    
    public int getRemotePort() {
        // TODO Auto-generated method stub
        return 0;
    }
    
    public int getLocalPort() {
        // TODO Auto-generated method stub
        return 0;
    }

    
}
