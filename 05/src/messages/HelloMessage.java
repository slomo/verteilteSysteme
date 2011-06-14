package messages;

import java.util.Arrays;
import java.util.StringTokenizer;

public class HelloMessage extends ProtocollMessage {

	public final MessageType type = MessageType.HELLO;
	public String name;

	public HelloMessage(String name) {
		this.name = name;
	}

	@Override
	public byte[] getData() {
		return makeMessage(type, Arrays.asList(name));
	}

	@Override
	public int getLength() {
		return getData().length;
	}


	public static HelloMessage parse(String s) {
		StringTokenizer token = new StringTokenizer(s);
		token.nextToken(); 
		return new HelloMessage(token.nextToken());
	}

    @Override
    public MessageType getType() {
        return this.type;
    }
	
	

}
