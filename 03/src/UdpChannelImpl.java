import java.io.IOException;
import java.net.InetAddress;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedChannelException;
import java.nio.channels.DatagramChannel;
import java.util.concurrent.BlockingQueue;

import vsFramework.Channel;
import vsFramework.Message;



public class UdpChannelImpl implements Channel, UdpChannel {

	private InetSocketAddress remote;
	int localPort;
	DatagramChannel ch;
	boolean closed = false;
	
	BlockingQueue<Message> rq = new java.util.concurrent.LinkedBlockingQueue<Message>();
	
	UdpChannelImpl(int localPort,InetSocketAddress remoteAddress, DatagramChannel ch) {
		this.localPort = localPort;
		this.remote = remoteAddress;
		this.ch = ch;
	}
	
	/* (non-Javadoc)
	 * @see UdpChannel#send(vsFramework.Message)
	 */
	@Override
	public void send(Message m) {
		  ByteBuffer out = ByteBuffer.wrap(m.getData());
	      try {
			ch.send(out, remote);
		} catch (ClosedChannelException e) {
			this.close();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	/* (non-Javadoc)
	 * @see UdpChannel#recv()
	 */
	@Override
	public Message recv() {
		try {
			return rq.take();
		} catch (InterruptedException e) {
			return null;
		}
	}
	
	/* (non-Javadoc)
	 * @see UdpChannel#nrecv()
	 */
	@Override
	public Message nrecv() {
		return rq.poll();
	}

	/* (non-Javadoc)
	 * @see UdpChannel#close()
	 */
	@Override
	public void close() {
		UdpChannelFactory.closeChannel(this);
		closed = true;
	}

	/* (non-Javadoc)
	 * @see UdpChannel#hasBeenClosed()
	 */
	@Override
	public boolean hasBeenClosed() {
		return closed;
	}
	
	
	/* (non-Javadoc)
	 * @see UdpChannel#getRemoteAddress()
	 */
	@Override
	public InetAddress getRemoteAddress() {
		// TODO Auto-generated method stub
		return remote.getAddress();
	}
	
	/* (non-Javadoc)
	 * @see UdpChannel#getRemotePort()
	 */
	@Override
	public int getRemotePort() {
		return remote.getPort();
	}
	
	/* (non-Javadoc)
	 * @see UdpChannel#getLocalPort()
	 */
	@Override
	public int getLocalPort() {
		return localPort;
	}

	
}
