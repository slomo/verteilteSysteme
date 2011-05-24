package vsFramework;

public abstract class AbstractMessage implements Message {

	byte data[];
		
	@Override
	public int getLength() {
		return data.length;
	}

	@Override
	public byte[] getData() {
		return data;
	}

}
