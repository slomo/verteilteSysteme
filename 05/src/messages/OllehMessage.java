package messages;

import java.util.Arrays;

public class OllehMessage extends HelloMessage {
	
    protected final MessageType type = MessageType.OLLEH;
    
	public OllehMessage(String name) {
		super(name);
	}
	
	@Override
    public byte[] getData() {
        return makeMessage(type, Arrays.asList(name));
    }
}
