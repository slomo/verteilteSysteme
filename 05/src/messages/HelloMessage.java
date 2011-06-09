package messages;

import java.util.Arrays;

public class HelloMessage extends ProtocollMessage {

	protected final MessageType type = MessageType.HELLO;
	public final String name;

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
	
	

}
