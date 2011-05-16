package solutions02;
import java.net.InetAddress;

import vsFramework.Message;

public interface UdpChannel extends vsFramework.Channel{

	public abstract void send(Message m);

	public abstract Message recv();

	public abstract Message nrecv();

	public abstract void close();

	public abstract boolean hasBeenClosed();

	/**
	 * @return the remote address we are connected to.
	 */
	public abstract InetAddress getRemoteAddress();

	public abstract int getRemotePort();

	public abstract int getLocalPort();

}