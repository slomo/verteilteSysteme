import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;


public class UdpChannelFactory {
    
    /**
     * static is not that fine, so we are using a singelton like pattern of our own instanze;
     */
    
    
    public static final int PACKAGE_SIZE = 512; 
    
    protected static UdpChannelFactory instance = null;
    
    protected UdpPacketDispatcher dispatcher;
    
    protected static UdpChannelFactory getSingelton(){
        
        // make singelton threadsafe
        synchronized(instance){
            if(instance == null){
                instance = new UdpChannelFactory();
            }
        }
        
        return instance;
    }
    
    
    /**
     * Builds a new UDP channel which may be used so receive a first packet from an arbitrary endpoint and send further messages to it
     * multiple calls to this share a socket 
     * @param local_port port to listen on
     * @return
     */
    public static UdpChannel newUdpChannel(int local_port) {
       return getSingelton().newUdpChannelIntern(local_port);
        
    }

    /**
     * Builds a new UDP channel which may be used for sending and receiving messages to a specified endpoint
     * @param local_port port to listen on
     * @param remote_addr address to connect to
     * @param remote_port port to connecht to 
     * @return
     */
    public static UdpChannel newUdpChannel(int local_port, InetSocketAddress remote_addr, int remote_port ) {
        return getSingelton().newUdpChannelIntern(local_port,remote_addr,remote_port);
    }

    
    /**************************************************************************
     * Non static begins here
     */
    
    protected Selector selector;
    
    // maps localport to channel listing on this port
    // TODO: Innere liste sollte vermutlich synchronized sein, da mehrer Threads schreiben
    protected Map <Integer,List<UdpChannel>> subscribers;
    
    public UdpChannelFactory(){
        
        try {
            selector = Selector.open();
        } catch (IOException e) {
            log("IOExeption, when creating Selector");
            e.printStackTrace();
        }
        
        
        
        
        
        
    }
    
    
    private UdpChannel newUdpChannelIntern(int localPort) {
     
        if(!subscribers.containsKey(localPort)) {
       
            try {
              
                dispatcher.addListner(localPort);
                
            } catch (IOException e) {
                log("Error creating channel");
                e.printStackTrace();
            }
            
            subscribers.put(localPort, new LinkedList<UdpChannel>());    
        }
        
        UdpChannel uc = new UdpChannel(localPort);
        subscribers.get(localPort).add(uc);
        return uc;      
    }
    
    
    private UdpChannel newUdpChannelIntern(int localPort, InetSocketAddress remoteAddr, int remotePort) {
        // TODO Auto-generated method stub
        return null;
        
    }
    
    public void notifyUdpChannels(int localPort, SocketAddress remoteAdress, int remotePort, ByteBuffer bb) {
        // TODO Auto-generated method stub
        
    }
    
    
    protected void log(String mesg){
        System.err.println("LOG[" + this.getClass().getName() + "]: " + mesg);
    }


       
}
