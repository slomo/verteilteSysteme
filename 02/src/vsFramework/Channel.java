package vsFramework;

/**
 * Basic communication channel with instant send and blocking receive
 * @author Philipp Schmidt <philipp.schmidt@fu-berlin.de>
 *
 */
public interface Channel {

	/**
	 * send a message to the channel - returns anyway
	 */
	public void send(Message m);
	
	/**
	 * receive a message from a channel - blocks till a new message is received
	 * @return a message
	 */
	public Message recv();
	
	/**
	 * close the channel to signal you do not want to send anything.
	 */
	public void close();
	
	/**
	 * checks if the channel has been closed by the other side
 	 * there might be still messages waiting for you to receive
	 * @return
	 */
	public boolean hasBeenClosed();

}
