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

        if (mesg != null) {
            if (chooseMyself) {
                // i choose my self as leader before and propagated my lead =>
                // just check sanity
                System.out.println("Round[" + round + "]:" +"Leader is now me: " + id);
                assert(mesg.getData()[0] == id && round > id);
                return null;
            } else {
                acceptedOther = true;
                // some one other won, just spreading the word
                System.out.println("Round[" + round + "]:" +"Forwarding for " + mesg.getData()[0] +  ": " + id);
                return mesg;
            }
        } else {
            if (round == id && ! acceptedOther){
                chooseMyself = true;
                // my turn i am going to win
                System.out.println("Round[" + round + "]:" +"Trying to be leader: " + id);
                return new ByteMessage(id);
            } else {
                // wait for it
                return null;
            }
        }
    }

}
