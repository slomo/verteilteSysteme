import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.net.DatagramSocket;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.Queue;
import java.nio.channels.Selector;
import java.util.Iterator;

import vsFramework.ByteArrayMessage;


public class UdpChannelFactory {

	static HashMap<Integer, DatagramChannel> activeDatagramChannels = new HashMap<Integer, DatagramChannel>();
	static HashMap<Integer, UdpChannelDispatcher> activeChannelDsipatchers = new HashMap<Integer, UdpChannelDispatcher>();
	static Selector readSelector = null;
	static Thread readThread = null;
	static Queue<DatagramChannel> newDatagramChannels = new LinkedList<DatagramChannel>();
	static Queue<UdpChannelImpl> abendonedUdpChannels = new LinkedList<UdpChannelImpl>();

	
	/**
	 * Builds a new UDP channel which may be used so receive a first packet from an arbitrary endpoint and send further messages to it
	 * multiple calls to this share a socket 
	 * @param local_port port to listen on
	 * @return
	 */
	public static UdpChannel newUdpChannel(int local_port) {
		
		
		try {
			
			UdpChannelDispatcher cd = getPortDispatcher(local_port);
			
			return cd.getNewWildcardChannel();
			
			
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return null;
	}


	/**
	 * Builds a new UDP channel which may be used for sending and receiving messages to a specified endpoint
	 * @param local_port port to listen on
	 * @param remote_addr address to connect to
	 * @param remote_port port to connecht to 
	 * @return
	 */
	public static UdpChannel newUdpChannel(int local_port, InetAddress remote_addr, int remote_port ) {
		
		try {
			
			UdpChannelDispatcher cd = getPortDispatcher(local_port);
			InetSocketAddress sa = new InetSocketAddress(remote_addr, remote_port);
			UdpChannelImpl ch = null;

			
			ch = cd.getChannel(sa);
			
			if (ch == null) {
				ch = new UdpChannelImpl(local_port, sa, activeDatagramChannels.get(local_port));
				cd.addChannel(ch, sa);
			}
			
			return ch;
						
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return null;
	}
	
	static void closeChannel (UdpChannelImpl ch) {
		
		synchronized (abendonedUdpChannels) {
			abendonedUdpChannels.add(ch);
		}
		readSelector.wakeup();		
	}
	
	
	private synchronized static UdpChannelDispatcher getPortDispatcher(final int local_port) throws IOException {
		
		UdpChannelDispatcher cd = activeChannelDsipatchers.get(local_port);
		
		if(cd != null) 
			return cd;
		
		// create socket
	    final SocketAddress address = new InetSocketAddress(local_port);
	    final DatagramChannel channel = DatagramChannel.open();
	    final DatagramSocket socket = channel.socket();
	    socket.bind(address);
	    channel.configureBlocking(false);
	    
	    // make sure that reader thread is up and running
	    checkReaderThread();
	    
	    // register new channel in factory
	    cd = new UdpChannelDispatcher(local_port);
	    activeDatagramChannels.put(local_port, channel);
	    activeChannelDsipatchers.put(local_port, cd);
	    
	    // tell reader set to add to read set
	    synchronized(newDatagramChannels) {
	    	newDatagramChannels.add(channel);
	    }
	    readSelector.wakeup();
	    
	    System.err.println("Newly bound to UDP socket" + address);
	    
	    return cd;
	    
	}
	
	private static void checkReaderThread() {
		
		if(readSelector == null) {
			try {
				readSelector = Selector.open();
			} catch (IOException e) {
				e.printStackTrace();
				return;
			}
	
			readThread = new Thread(new Runnable () {

				final ByteBuffer buf = ByteBuffer.allocate(8192);

				@Override
				public void run() {

					while(true)	try {
						
						//register new channels
						synchronized(newDatagramChannels) {
							DatagramChannel nch;
							while((nch = newDatagramChannels.poll()) != null) {
								nch.register(readSelector, SelectionKey.OP_READ);
							}
						}
						
						// close old channels
						synchronized (abendonedUdpChannels) {
							UdpChannelImpl uch; 
							while((uch = abendonedUdpChannels.poll()) != null) {
								UdpChannelDispatcher disp = activeChannelDsipatchers.get(uch.getLocalPort());
								if(disp.removeChannel(uch) <= 0) {
									// close channel
									activeDatagramChannels.remove(uch.ch);
									uch.ch.close();
									activeChannelDsipatchers.remove(disp);
									return;
								}
							}
						}
						
						// do requests
						if(readSelector.select()<0) continue;

						Iterator<SelectionKey> it = readSelector.selectedKeys().iterator();

						while(it.hasNext()) {
							
							// look if there is work to do
							SelectionKey key = it.next();
							if(!key.isReadable())
								continue;
							
							// read datagram
							buf.clear();
							DatagramChannel ch = (DatagramChannel) key.channel();
							InetSocketAddress client = (InetSocketAddress) ch.receive(buf);
							
							if(client == null) continue; // drop message

							// get dispacher and channel
							UdpChannelDispatcher disp = activeChannelDsipatchers.get(ch.socket().getLocalPort());
							UdpChannelImpl recv = disp.dispatch(client);
							if(recv == null) continue; // drop message
							
							// create message						
							assert(buf.hasArray());
							ByteArrayMessage msg = new ByteArrayMessage(buf.array(), buf.position());
							
							//System.err.println("UDP Packet from "+client.toString()+" "+buf.position()+" bytes payload:"+new String(msg.data));
							
							recv.rq.add(msg);

						}
					}catch (IOException ex) {
							System.err.println(ex);
					}	
				}
			});
			readThread.start();
		}
	}
}
