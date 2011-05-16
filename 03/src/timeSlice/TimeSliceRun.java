package timeSlice;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.ArrayList;
import java.util.List;

import solutions02.UdpChannelFactory;
import syncFramework.IpNodeId;
import syncFramework.Sequenzer;
import vsFramework.Channel;

public class TimeSliceRun {
    
    
    public static void main(String args[]){
        
        int[] ids = {3,5,8};
        
        InetAddress localhost = null;
        try {
            localhost = InetAddress.getByName("localhost");
        } catch (UnknownHostException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        
        
        List<IpNodeId> ipIds
        
        for(int i : ids){
            
            
            
        }
        
        int port1 = 1100;
        int port2 = 2200;
        int port3 = 3300;
        
        IpNodeId id1 = new IpNodeId(localhost,port1);
        IpNodeId id2 = new IpNodeId(localhost,port2);
        IpNodeId id3 = new IpNodeId(localhost,port3);
        
        
        IpNodeId cId = new IpNodeId(localhost,1025);
        
        
        TimeSliceNode n1 = new TimeSliceNode(id2,id3,cId,port1,(byte) 3);
        TimeSliceNode n2 = new TimeSliceNode(id3,id1,cId,port2,(byte) 5);
        TimeSliceNode n3 = new TimeSliceNode(id1,id2,cId,port3,(byte) 8);
        
        
        new Thread(n1).start();
        new Thread(n2).start();
        new Thread(n3).start();
        
        
        List<Channel> channels = new ArrayList<Channel>();
        
        
        channels.add(UdpChannelFactory.newUdpChannel(1025, id1.getAddr(), id1.getPort()));
        channels.add(UdpChannelFactory.newUdpChannel(1025, id2.getAddr(), id2.getPort()));
        channels.add(UdpChannelFactory.newUdpChannel(1025, id3.getAddr(), id3.getPort()));
        
        Sequenzer s = new Sequenzer(channels);
        
        
        for(int i=1;i<100;i++){
            s.performRound((byte)i);
        }
        
        
    }

}
