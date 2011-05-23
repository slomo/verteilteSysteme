package udp;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import udp.models.UdpMessage;
import vsFramework.Message;

public class UdpDispatcher implements Runnable {

    public static final int PAKET_SIZE = 512;

    Selector selector;
    UdpTable table;
    Map<SocketAddress, DatagramChannel> channels;

    public UdpDispatcher() {
        try {
            selector = Selector.open();
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
        table = new UdpTable();
        channels = new HashMap<SocketAddress, DatagramChannel>();

    }

    @Override
    public void run() {
        for (;;) {
            try {
                dispatchForAllSockets();
            } catch (IOException e) {
                log("Error when dispatching package");
                e.printStackTrace();
            }
        }
    }

    protected void dispatchForAllSockets() throws IOException {

        if (selector.select() <= 0) { // no events incoming
            return;
        }

        Set<SelectionKey> selectedKeys = selector.selectedKeys();

        for (SelectionKey key : selectedKeys) {

            selectedKeys.remove(key);

            if (key.isReadable()) {

                ByteBuffer bb = ByteBuffer.allocate(PAKET_SIZE);
                DatagramChannel dc = (DatagramChannel) key.channel();
                dc.read(bb);

                SocketAddress local = dc.socket().getLocalSocketAddress();
                SocketAddress remote = dc.socket().getRemoteSocketAddress();

                if (remote == null) {
                    log("error, remote was null");
                }

                notifyUdpChannel(local, remote, bb);

            } else {
                log("Key is somithing else, bizarre");
            }

        }

    }

    protected void notifyUdpChannel(SocketAddress local, SocketAddress remote, ByteBuffer bb) {

        Message m = new UdpMessage(bb);

        Set<UdpChannel> listners = table.getListners(local);
        Set<UdpChannel> unbound = new HashSet<UdpChannel>();

        // match channels with correct remote first
        for (UdpChannel chan : listners) {
            if (chan.isClosed) {
                listners.remove(chan);
                continue;
            }

            if (chan.getRemoteAddress() == null) {
                unbound.add(chan);
                continue;
            }

            if (remote.equals(chan.getRemoteAddress())) {
                chan.queue.add(m);
                return;
            }
        }

        log("There are " + unbound.size() + " unbound chans.");
        if (unbound.size() > 0) {
            UdpChannel u = unbound.iterator().next();
            u.queue.add(m);
            u.remote = (InetSocketAddress) remote;
            return;
        }

        log("Dropping message no one wants");
    }

    public void register(UdpChannel chan) {

        if (!channels.containsKey(chan.getLocalAddress())) {
            try {
                addPortToSelector(chan.getLocalAddress());
            } catch (IOException e) {
                log("Unable to open ports on socket: " + chan.getLocalPort());
                e.printStackTrace();
            }
        }

        table.addListner(chan.getLocalAddress(), chan);

    }

    public void addPortToSelector(InetSocketAddress local) throws IOException {
        DatagramChannel chan;
        chan = DatagramChannel.open();
        chan.configureBlocking(false);
        chan.socket().bind(local);
        chan.register(selector, SelectionKey.OP_READ);

    }

    protected void log(String mesg) {
        System.err.println("LOG[" + this.getClass().getName() + "]: " + mesg);
    }

    public void send(Message m, UdpChannel chan) {
        DatagramChannel dc = channels.get(chan.getLocalAddress());
        ByteBuffer bb = ByteBuffer.wrap(m.getData());
        try {
            dc.send(bb, chan.getRemoteAddress());
        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }
    }

}
