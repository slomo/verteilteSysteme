package syncFramework;
import solutions02.UdpChannelFactory;
import vsFramework.Channel;
import vsFramework.Message;


public abstract class AbstractNode implements Node {
    
    Channel controllerChan;
    Channel next;
    Channel prev;
    
    boolean cond = true;
    
    public AbstractNode(IpNodeId next, IpNodeId prev, IpNodeId seq, int localBasePort) {
        
        this.controllerChan = UdpChannelFactory.newUdpChannel(localBasePort, seq.getAddr(), seq.getPort());
        this.next = UdpChannelFactory.newUdpChannel(localBasePort, next.getAddr(), next.getPort());
        this.prev = UdpChannelFactory.newUdpChannel(localBasePort, prev.getAddr(), prev.getPort());
        
        System.err.println("Node listning for commands on " + seq);
    }
    
    public void stop(){
        cond = false;
    }

    @Override
    public void run() {
        while(cond){
            // wait for start of round
            Message m = controllerChan.recv();
            
            // get message produced last round
            Message in = prev.nrecv();
            
            // compute next message
            Message out = this.nextRound(m.getData()[0],in);
            
            // signal and wait for other processes
            controllerChan.send(m);
            m = controllerChan.recv();
           
            // send output
            if(out != null){
                next.send(out);
            }
            
            // notify 
            controllerChan.send(m);
        }
    }
    


    @Override
    public abstract Message nextRound(Byte round, Message mesg);
}
