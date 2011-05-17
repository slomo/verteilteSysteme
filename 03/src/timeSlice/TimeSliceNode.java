package timeSlice;

import syncFramework.AbstractNode;
import syncFramework.IpNodeId;
import vsFramework.ByteMessage;
import vsFramework.Message;

public class TimeSliceNode extends AbstractNode {

    public TimeSliceNode(IpNodeId next, IpNodeId prev, IpNodeId seq, int localBasePort, byte id) {
        super(next, prev, seq, localBasePort);
        this.id = id;
    }

    byte id;
    boolean chooseMyself = false;
    boolean acceptedOther = false;

    @Override
    public Message nextRound(Byte round, Message mesg) {

      
        // got message
        if (mesg != null) {
            byte content = mesg.getData()[0];

            if (chooseMyself) {
                if (content < id) {
                    System.out.println("Round[" + round + "]:" + "Giving up in favor of " + content + " says " + id);
                }

                if (content > id) {
                    System.out.println("Round[" + round + "]:" + "To big, dumping " + content + " says " + id);
                    return null;
                }
                
                if (content == id) {
                    System.out.println("Round[" + round + "]:" + "Leader is now me says " + id);
                    return null;
                }
            } else {
                System.out.println("Round[" + round + "]:" + "Forwarded " + content + " says " + id);
                acceptedOther = true;
            }
            return mesg;
        } else {
            // it is my turn
            if (round == id && !acceptedOther) {
                System.out.println("Round[" + round + "]:" + "Trying to be leader says " + id);
                chooseMyself = true;
                return new ByteMessage(id);
            } else {
                return null;
            }
        }
        
        // default: forward, what you got or didn't got
       
    }

}
