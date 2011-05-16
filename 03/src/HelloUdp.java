import java.net.InetAddress;
import java.net.UnknownHostException;

import vsFramework.*;


public class HelloUdp {



		// and add some fun for the endpoints 
		
		public static void main(String[] args) throws InterruptedException {
			
			Channel senderChannel;
			
			System.err.println("Starting receiver");
			receiver.start();
			
			
			System.err.println("Creating sender channel");

			try {
				senderChannel = UdpChannelFactory.newUdpChannel(4223, InetAddress.getByName("localhost"), 2342);

				Thread.sleep(2000);

				System.err.println("Sending message");
				for(Message m: ByteMessage.fromString("Hello World!\0")) {
					senderChannel.send(m);
				}

				System.err.println("Closing channel");
				senderChannel.close();
			
			} catch (UnknownHostException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}


			receiver.join();
			System.err.println("\nReceiver finished");
			System.exit(0);

			
		}
		
		static Thread receiver = new Thread() {
			public void run() {
				Channel receiverChannel = UdpChannelFactory.newUdpChannel(2342);
				while(true) {
					Message m = receiverChannel.recv();
					if(m != null) {
						System.out.print(m.toString());
						if(m.getData()[0] == 0) {
							return;
						}
					} else if (receiverChannel.hasBeenClosed()) {
						return;
					} else {
						System.err.println("receiver error!");
					}	
				}

			}
		};

	


}
