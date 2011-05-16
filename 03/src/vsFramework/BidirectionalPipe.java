package vsFramework;
import java.util.LinkedList;

/**
 * A Bidirectional pipe generates two channels, where a message sent via one channel is received on the other 
 * @author Philipp Schmidt <philipp.Schmidt@fu-berlin.de>
 *
 */
public class BidirectionalPipe {

	abstract class PipeChannel implements Channel {
		
		LinkedList<Message> queue = new LinkedList<Message>();
		boolean closed = false;
		
		@Override
		public synchronized void send(Message m) {
				queue.add(m);
				this.notify();
		}
		
		@Override
		public synchronized void close() {
			closed = true;
			this.notifyAll();
		}
				
	};	
	
	PipeChannel left = new PipeChannel() {

		@Override
		public Message recv() {
			synchronized (right) {
				while(right.queue.isEmpty()) {
					if(right.closed)
						return null;
					try {
						right.wait();
					} catch (InterruptedException e) {
						e.printStackTrace();
						return null;
					}
				}
				return new ByteArrayMessage(right.queue.remove().getData());
			}
		}
		
		@Override
		public Message nrecv() {
			synchronized (right) {
				if(right.queue.isEmpty()) 
					return null;
				return new ByteArrayMessage(right.queue.remove().getData());
			}

		}
		
		public boolean hasBeenClosed() {
			return right.closed;
		}



	};
	
	PipeChannel right = new PipeChannel(){

		@Override
		public Message recv() {
			synchronized (left) {
				while(left.queue.isEmpty()) {
					if(left.closed) {
						return null;
					} else	try {
						left.wait();
					} catch (InterruptedException e) {
						e.printStackTrace();
						return null;
					}
				}
				return new ByteArrayMessage(left.queue.remove().getData());
			}
		}
		
		@Override
		public Message nrecv() {
			synchronized (left) {
				if(left.queue.isEmpty()) 
					return null;
				return new ByteArrayMessage(left.queue.remove().getData());
			}

		}
		
		public boolean hasBeenClosed() {
			return left.closed;
		}

		
	};
	
	/**
	 * @return left side of the pipe
	 */
	public Channel gehtLeft() {
		return left;
	}
	
	/**
	 * @return right side of the pipe
	 */
	public Channel gehtRight() {
		return right;
	}

	
}
