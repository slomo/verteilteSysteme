package solutions02;
import vsFramework.Channel;
import vsFramework.Message;

/**
 * 
 */

/**
 * @author 
 *
 */
public class AlternatingBitSenderProxy implements Channel {

	Channel c;
	
	public AlternatingBitSenderProxy (Channel c) {
		// TODO Auto-generated method stub
		System.err.println("Warning: AlternatingBitSenderProxy is still a dummy and won't help!");
		this.c = c;
	}
	
	/* (non-Javadoc)
	 * @see vsFramework.Channel#send(vsFramework.Message)
	 */
	@Override
	public void send(Message m) {
		// TODO Auto-generated method stub
		c.send(m);
	}

	/* (non-Javadoc)
	 * @see vsFramework.Channel#recv()
	 */
	@Override
	public Message recv() {
		return null;
	}
	
	/* (non-Javadoc)
	 * @see vsFramework.Channel#recv()
	 */
	@Override
	public Message nrecv() {
		return null;
	}

	/* (non-Javadoc)
	 * @see vsFramework.Channel#close()
	 */
	@Override
	public void close() {
		// TODO Auto-generated method stub
		c.close();
	}

	/* (non-Javadoc)
	 * @see vsFramework.Channel#hasBeenClosed()
	 */
	@Override
	public boolean hasBeenClosed() {
		// TODO Auto-generated method stub
		return c.hasBeenClosed();
	}

}
