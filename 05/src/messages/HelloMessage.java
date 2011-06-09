package messages;

import java.util.Arrays;
import java.util.StringTokenizer;

public class HelloMessage extends ProtocollMessage {

	protected final MessageType type = MessageType.HELLO;
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

	@Override
	public void parse(String s) {
		StringTokenizer token = new StringTokenizer(s);
		token.nextToken();
		name = token.nextToken();
	}
	
	

}
