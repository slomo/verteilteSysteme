package vsFramework;

public abstract class AbstractMessage implements Message {

	public byte data[];
		
	@Override
	public int getLength() {
		return data.length;
	}

	@Override
	public byte[] getData() {
		return data;
	}

	public String toString() {
		return new String(data);
	}
	
}
