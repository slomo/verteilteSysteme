package udp;

import java.net.SocketAddress;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

public class UdpTable {
    
    Map<Integer, Set<UdpChannel>> listners = new HashMap<Integer,Set<UdpChannel>>();
    
  /**  Map<UdpChannel,InetSocketAddress> senders = new HashMap<UdpChannel,InetSocketAddress>();
    Set<UdpChannel> unboundListner = new HashSet<UdpChannel>();
    **/
    public void addListner(int port, UdpChannel chan) {
        Set<UdpChannel> set = listners.get(port);
        
        if(set == null){
            set = new HashSet<UdpChannel>();
            listners.put(port, set);
        }
        
        set.add(chan);
    }
    
    public void removeListner(SocketAddress local, UdpChannel chan){
        Set<UdpChannel> set = listners.get(local);
        set.remove(local);   
        if(set.isEmpty()){
            listners.remove(local);
        }
    }
    
    public  Set<UdpChannel> getListners(Integer port) {
        return listners.get(port);
    }
    /**
    public Set<UdpChannel> getUnboundListnerSet() {
        return unboundListner;
    }
    public Map<UdpChannel,InetSocketAddress> getSenderMap() {
        return senders;
    }
    */
    
    

}
