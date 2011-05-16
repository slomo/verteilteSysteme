package vsFramework;
import java.util.Random;

/**
 * A channel proxy randomly dropping a certain fraction of all messages sent through it
 * No messages received through the proxy will be harmed.
 * @author Philipp Schmidt <philipp.schmidt@fu-berlin.de>
 *
 */
public class LossyChannelProxy implements Channel {

	Channel c;
	double lossrate;
    private static Random rnd = new Random();
	
	/**
	 * 
	 * @param c			Channel to be proxy for
	 * @param lossrate	Packet drop ratio (1 drops all packets, 0 drops none)
	 */
	public LossyChannelProxy(Channel c, double lossrate) {
		this.c = c;
		this.lossrate = lossrate;
	}
	


	@Override
	public void send(Message m) {
		double d = rnd.nextDouble();
		if( d > lossrate) {
			c.send(m);
		}
	}

	@Override
	public Message recv() {
		return c.recv();
	}
	
	@Override
	public Message nrecv() {
		return c.nrecv();
	}
	
	@Override
	public void close() {
		c.close();
	}

	@Override
	public boolean hasBeenClosed() {
		return c.hasBeenClosed();
	}

}
