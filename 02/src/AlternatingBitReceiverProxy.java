import vsFramework.Channel;
import vsFramework.Message;

/**
 * 
 */

/**
 * @author 
 *
 */
public class AlternatingBitReceiverProxy implements Channel {

	Channel c;
	
	public AlternatingBitReceiverProxy (Channel c) {
		// TODO Auto-generated method stub
		System.err.println("Warning: AlternatingBitReceiverProxy is still a dummy and won't help!");
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
		// TODO Auto-generated method stub
		return c.recv();
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
