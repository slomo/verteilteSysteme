package messages;

public enum MessageType {
	
	HELLO(HelloMessage.class),
	OLLEH(OllehMessage.class),
	
	PEERS(HelloMessage.class),
	SREEP(HelloMessage.class);
	
	public Class<?> impl;
	
	MessageType(Class<?> impl){
		this.impl = impl;
	}
}
