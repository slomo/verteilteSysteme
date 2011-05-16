package syncFramework;

import java.net.InetAddress;

public class IpNodeId {
       
    protected int port;
    protected InetAddress addr;
    
    public IpNodeId(InetAddress addr, int port) {
        this.port = port;
        this.addr = addr;
    } 

    public int getPort() {
        return port;
    }
    public InetAddress getAddr() {
        return addr;
    }

    @Override
    public String toString() {
        return "IpNodeId [addr=" + addr + ", port=" + port + "]";
    }
    
    
    
}
