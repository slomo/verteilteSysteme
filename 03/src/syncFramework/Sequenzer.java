package syncFramework;

import java.util.List;

import vsFramework.ByteMessage;
import vsFramework.Channel;
import vsFramework.Message;

public class Sequenzer {
    
        List<Channel> nodes;
    
        // Nodes must be connected
        public Sequenzer(List<Channel> nodes){
            this.nodes = nodes;
        }
        
        // simpel implementation of a process barrier
        public void performRound(byte number){
            
            Message m = new ByteMessage(number);
            
            // tell nodes that round started
            for( Channel n : nodes){
                n.send(m);
            }
            
            // wait for computation of new message
            for( Channel c : nodes){
                Message n = c.recv();
                assert(n.getData()[0] == number);
            }
            
            // tell nodes to write output
            for( Channel n : nodes){
                n.send(m);
            }
            
            // wait for till all nodes are done
            for( Channel c : nodes){
                Message n = c.recv();
                assert(n.getData()[0] == number);
            }
            
            
        }

}
