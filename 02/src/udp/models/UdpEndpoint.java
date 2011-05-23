package udp.models;
import java.net.SocketAddress;


public class UdpEndpoint {
    
    protected final int port;
    protected final SocketAddress socketAddress;
    
    public UdpEndpoint(int port, SocketAddress socketAddress) {
        this.port = port;
        this.socketAddress = socketAddress;
    }
    
    public int getPort() {
        return port;
    }
    public SocketAddress getSocketAddress() {
        return socketAddress;
    }
    

}
