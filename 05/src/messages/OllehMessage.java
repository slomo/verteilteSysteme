package messages;

public class OllehMessage extends HelloMessage {
	
	public OllehMessage(String name) {
		super(name);
	}

	protected final MessageType type = MessageType.OLLEH;
	
}
