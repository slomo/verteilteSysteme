package syncFramework;
import vsFramework.Message;


public interface Node extends Runnable{

    /*
     * Message might be null, if no message was reviecved.
     * if return value is null no message will be send
     */
    public Message nextRound(Byte round,Message mesg);
    
    public void stop();
}
