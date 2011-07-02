package udp.models;
import java.nio.ByteBuffer;

import vsFramework.Message;



public class UdpMessage implements Message {
    

    protected byte[] data;
    
    
    public UdpMessage(byte[] b){
        data = b;
    }
    
     public UdpMessage(ByteBuffer bb) {
        data = new byte[bb.position()];
        bb.rewind();
        bb.get(data);
    }

    @Override
    public byte[] getData() {
        return data;
    }

    @Override
    public int getLength() {
        return data.length;
    }

}
