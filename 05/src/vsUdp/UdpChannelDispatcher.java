package vsUdp;
import java.net.InetSocketAddress;
import java.util.HashMap;
import java.util.concurrent.BlockingQueue;

	class UdpChannelDispatcher{
		
		int port;
		final HashMap<InetSocketAddress, UdpChannelImpl> active = new HashMap<InetSocketAddress, UdpChannelImpl>();
		final BlockingQueue<UdpChannelImpl> wait_gen = new java.util.concurrent.LinkedBlockingQueue<UdpChannelImpl>();
		volatile int waiting = 0;
		
		public UdpChannelDispatcher(int port) {
			this.port = port;
		}
		
		/**
		 * get a channel for an endpoint or a waiting wildcard channel
		 * @param client endpoint
		 * @return channel associated to this endpoint
		 */
		public synchronized UdpChannelImpl dispatch(InetSocketAddress client) {

			UdpChannelImpl c = null;

			if( (c = active.get(client)) == null ) {

				// new client - is someone waiting?
				if(waiting > 0) {
					c = (UdpChannelImpl) UdpChannelFactory.newUdpChannel(port, client.getAddress(), client.getPort());
					System.err.println("Got new Client - dispatching to waiting channel");
					wait_gen.add(c);
					waiting--;
				} else {
					System.err.println("Got new Client - ignoring because of no waiting channel");
				}
				
			}
			return c;
		}
		
		/**
		 * get a channel for an endpoint
		 * @param s endpoint
		 * @return the channel
		 */
		public synchronized UdpChannelImpl getChannel (InetSocketAddress s) {
			return active.get(s);
		}
		
		/** 
		 * Add a new channel to the active channel set
		 * @param c channel to add
		 * @param s socket address to accept packets from
		 */
		public synchronized void addChannel (UdpChannelImpl c, InetSocketAddress s) {
			active.put(s, c);
		}
		
		/**
		 * get a new unassociated channel
		 * blocking till a packet without an associated channel arrives
		 * @return
		 */
		public UdpChannel getNewWildcardChannel () {
			UdpChannel c = null; 
			synchronized(this) {
				waiting++;
			}
			try {
				return wait_gen.take();
			} catch (InterruptedException e) {
				e.printStackTrace();
			}
			synchronized(this) {
				waiting--;
			}
			return c;
		}
		


		/**
		 * Remove a channel from the active channel set
		 * @param ch channel to remove
		 * @return number of remaining active channels
		 */
		public synchronized int removeChannel(UdpChannel ch) {
			active.remove(ch);
			return active.size()+waiting;
		}
		
		
		
	}
