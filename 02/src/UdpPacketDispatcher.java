import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.Set;

public class UdpPacketDispatcher implements Runnable {

    Selector selector;
    UdpChannelFactory ucf;

    @Override
    public void run() {
        for(;;){
            dispatchForAllSockets();
        }
    }

    protected void dispatchForAllSockets() {
        try {
            while (selector.select() > 0) {

                Set<SelectionKey> selectedKeys = selector.selectedKeys();

                for (SelectionKey key : selectedKeys) {

                    selectedKeys.remove(key);

                    if (key.isReadable()) {

                        ByteBuffer bb = ByteBuffer.allocate(UdpChannelFactory.PACKAGE_SIZE);
                        DatagramChannel dc = (DatagramChannel) key.channel();
                        dc.read(bb);

                        int localPort = dc.socket().getPort();
                        int remotePort = 0;
                        SocketAddress remoteAdress = null;
                        
                        ucf.notifyUdpChannels(localPort,remoteAdress,remotePort,bb);

                    } else {
                        log("Key is somithing else, bizarre");
                    }

                }

            }

        } catch (IOException e) {
            // TODO Auto-generated catch block
            e.printStackTrace();
        }

    }

    protected void log(String mesg) {
        System.err.println("LOG[" + this.getClass().getName() + "]: " + mesg);
    }

    public void addListner(int localPort) throws IOException {
        DatagramChannel chan;
        chan = DatagramChannel.open();
        chan.configureBlocking(false);
        chan.socket().bind(new InetSocketAddress(localPort));
        chan.register(selector,SelectionKey.OP_READ);
    }

}
