package timeSlice;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

import solutions02.UdpChannelFactory;
import syncFramework.IpNodeId;
import syncFramework.Node;
import syncFramework.Sequenzer;
import vsFramework.Channel;

public class TimeSliceRun {
    
    
    @SuppressWarnings("deprecation")
    public static void main(String args[]){
        
        int[] ids = {3,2,1,5,6,4};
        
        InetAddress localhost = null;
        try {
            localhost = InetAddress.getByName("localhost");
        } catch (UnknownHostException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        IpNodeId cId = new IpNodeId(localhost,1024);
        
        List<IpNodeId> ipIds = new ArrayList<IpNodeId>();
        
        for(int i : ids){
            ipIds.add(new IpNodeId(localhost,i+1025));
        }

    
        List<Node> nodes = new ArrayList<Node>();
        
        for(int i=0;i<ids.length;i++){
            int next = (i + 1) % ids.length;
            int prev = ( i + ids.length - 1)% ids.length;
            
            nodes.add(new TimeSliceNode(
                    ipIds.get(next),
                    ipIds.get(prev),
                    cId,
                    ipIds.get(i).getPort(),
                    (byte)ids[i]));
            new Thread(nodes.get(i)).start();
        }

        List<Channel> channels = new ArrayList<Channel>();
        
        for( IpNodeId id : ipIds ){
            channels.add(UdpChannelFactory.newUdpChannel(1024, id.getAddr(), id.getPort()));
        }
        
        Sequenzer s = new Sequenzer(channels);
        
     
        for(int i=0;i<127;i++){
            s.performRound((byte)i);
        }
        
        for(Node node : nodes){
            node.stop();
        }
        
        return;
    }

}
