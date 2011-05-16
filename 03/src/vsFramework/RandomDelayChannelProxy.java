package vsFramework;

import java.util.PriorityQueue;
import java.util.Random;

public class RandomDelayChannelProxy implements Channel {

	
	Channel c;
	double delayMin;	 //minimum delay in seconds
	double delayVariant; //delay variation in seconds 

	final PriorityQueue<DelayedMessage> queue = new PriorityQueue<DelayedMessage>();
	private long last;		// time of the last event in queue
	private long count = 0;	// counter to retain order in case time is the same
	private long closingTime = 0; // time to close the channel

    private static Random rnd = new Random();

    /**
     * Delay packets sent through this proxy for a random amount of time
     * @param c			channel to proxy
     * @param delayMin	minimal delay to add in seconds
     * @param delayMax  maximal delay to add in seconds
     */
	public RandomDelayChannelProxy(Channel c, double delayMin, double delayMax) {
		
		this.c = c;
		this.delayMin =  delayMin;
		this.delayVariant = (delayMax-delayMin);
		
		sender.start();
	}

	@Override
	public void send(Message m) {
		
		long delay = 1000 * (long) (delayMin + delayVariant*rnd.nextDouble());
		
		if(delay <= 0) {
			// System.err.println("RandomDelayChannelProxy@"+System.currentTimeMillis()+" sending message " + m.toString() + " without delay");
			c.send(m);
		} else {
			synchronized(queue) {
				long when = System.currentTimeMillis()+delay;
				// System.err.println("RandomDelayChannelProxy@"+System.currentTimeMillis()+" adding message " + m.toString() + " delay " + delay + " at " + when + " last " + last);
				last = (last>when) ? last : when;
				queue.add(new DelayedMessage(m, when, count++));
				queue.notify();
			}
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
		
		synchronized(queue) {

			if(queue.isEmpty()) {
				// System.err.println("RandomDelayChannelProxy@"+System.currentTimeMillis()+" closing channel");
				closingTime = System.currentTimeMillis();
				c.close();				
			} else {
				// System.err.println("RandomDelayChannelProxy@"+System.currentTimeMillis()+" will close channel at "+last);
				closingTime = last;
			}
			queue.notifyAll();

		}
					
	}

	@Override
	public boolean hasBeenClosed() {
		return c.hasBeenClosed();
	}
	

	class DelayedMessage implements Comparable<DelayedMessage> {

		Long arrival;
		Long count;
		Message m;
		
		DelayedMessage(Message m, long arrival, long count) {
			this.arrival = arrival;
			this.m = m;
			this.count = count;
		}
		
		@Override
		public int compareTo(DelayedMessage o) {
			int c = arrival.compareTo(o.arrival);
			return c != 0 ? c : count.compareTo(o.count);
		}
		
	}
	
	Thread sender = new Thread() {
		public void run() {
			while(true)	{
				synchronized(queue) {
			
					long now = System.currentTimeMillis();
					long delay = 0;
					
					// try to deliver new messages
					while(!queue.isEmpty() && queue.peek().arrival <= now) {
						
						Message m = queue.remove().m;
						// System.err.println("RandomDelayChannelProxy$sender@"+now+" sending message " + m.toString());
						c.send(m);

					}
					
					// close the channel?
					if(closingTime != 0 && closingTime <= now) {
						//System.err.println("RandomDelayChannelProxy$sender@"+now+" closing channel");
						c.close();
						return;
					}
					
					// how long to sleep?
					if(!queue.isEmpty()) {	
						delay = queue.peek().arrival - now;
					}
					
					try { 
						// System.err.println("RandomDelayChannelProxy$sender@"+now+" sleeping for " +delay);
						queue.wait(delay);
					} catch (InterruptedException e) {
						e.printStackTrace();
					}
				}			
			}
		}
	};
}
