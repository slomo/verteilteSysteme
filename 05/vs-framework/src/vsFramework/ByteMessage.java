package vsFramework;

/**
 * A Message containing a single byte
 * @author Philipp Schmidt <philipp.schmidt@fu-berlin.de>
 *
 */
public class ByteMessage extends AbstractMessage {

	public ByteMessage (byte b) {
		data = new byte[1];
		data[0] = b;
	}
	
	/**
	 * Construct multiple byte messages from a String
	 * @param s String to split into byte messages
	 * @return array of ByteMessages representing the String s
	 */
	public static ByteMessage[] fromString (String s) {
		
		byte[] in = s.getBytes(); 
		ByteMessage[] out = new ByteMessage[in.length];
		                  
		for (int i = 0; i < in.length; i++) {
			out[i] = new ByteMessage(in[i]);
		}
		
		return out;
		
	}

	
}
