package messages;

import java.util.List;

import vsFramework.Message;

public abstract class ProtocollMessage implements Message {

	public static final char SEPERATOR = ' ';
	protected final MessageType type = null;
	
	@Override
	public abstract byte[] getData();
	
	@Override
	public abstract int getLength();
	
	public byte[] makeMessage(MessageType type, List<String> content){
		
		StringBuffer sb = new StringBuffer();
		
		sb.append(type.toString());
		
		for(String element : content){
			sb.append(SEPERATOR);
			sb.append(element);
		}
		
		return sb.toString().getBytes();
	}

	public MessageType getType() {
		return type;
	}
	
	public abstract void parse(String s);
	
}
