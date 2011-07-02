package udp;

import java.net.InetSocketAddress;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

import vsFramework.Channel;
import vsFramework.Message;


public class UdpChannel implements Channel {

    BlockingQueue<Message> queue = new LinkedBlockingQueue<Message>();
    
    UdpDispatcher dispatcher;
    InetSocketAddress local;
    InetSocketAddress remote;
    Boolean isClosed = false;
    
    public UdpChannel(InetSocketAddress local, InetSocketAddress remote, UdpDispatcher dispatcher){    
        this.dispatcher = dispatcher;
        this.local = local;
        this.remote = remote;
        dispatcher.register(this);
    }
    
    public UdpChannel(InetSocketAddress local, UdpDispatcher dispatcher){
        this(local,null,dispatcher);
    }
    
    
    
	@Override
	public void send(Message m) {
		dispatcher.send(m,this);
	}

	@Override
	public Message recv() {
		try {
            return queue.take();
        } catch (InterruptedException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
            return null;
        }
	}

	@Override
	public void close() {
		queue = null;
		isClosed = true;
	}

	@Override
	public boolean hasBeenClosed() {
		return isClosed;
	}
	
	/**
	 * @return the remote address we are connected to.
	 */
	public InetSocketAddress getRemoteAddress() {
		return remote;
	}
	
	public int getRemotePort() {
		return remote.getPort();
	}
	
	public int getLocalPort() {
		return local.getPort();
	}

	public InetSocketAddress getLocalAddress() {
        return local;
    }
	
}
