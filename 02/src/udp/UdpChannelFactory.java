package udp;

import java.net.InetAddress;
import java.net.InetSocketAddress;

public class UdpChannelFactory {

    static UdpDispatcher dispatcher = null;

    protected static void startIfNeeded() {
        if (dispatcher == null) {
            dispatcher = new UdpDispatcher();
            new Thread(dispatcher).start();
        }
    }

    /**
     * Builds a new UDP channel which may be used so receive a first packet from
     * an arbitrary endpoint and send further messages to it multiple calls to
     * this share a socket
     * 
     * @param local_port
     *            port to listen on
     * @return
     */
    public static UdpChannel newUdpChannel(int local_port) {

        InetSocketAddress local_addr = new InetSocketAddress(local_port);

        startIfNeeded();

        return new UdpChannel(local_addr, dispatcher);

    }

    /**
     * Builds a new UDP channel which may be used for sending and receiving
     * messages to a specified endpoint
     * 
     * @param local_port
     *            port to listen on
     * @param remote_addr
     *            address to connect to
     * @param remote_port
     *            port to connecht to
     * @return
     */
    public static UdpChannel newUdpChannel(int local_port, InetAddress remote_addr, int remote_port) {

        startIfNeeded();

        InetSocketAddress local_addr = new InetSocketAddress(local_port);
        InetSocketAddress remote = new InetSocketAddress(remote_addr, remote_port);

        return new UdpChannel(local_addr, remote, dispatcher);
    }

}
