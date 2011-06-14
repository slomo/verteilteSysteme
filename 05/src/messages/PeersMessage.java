package messages;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

public class PeersMessage extends ProtocollMessage {

    MessageType type = MessageType.PEERS;
    
    List<String> route;
    
    public PeersMessage(List<String> route){
        this.route = route;
    }
    
    @Override
    public byte[] getData() {       
        return makeMessage(type, route);
    }

    @Override
    public int getLength() {
        return getData().length;
    }

    @Override
    public MessageType getType() {
        return type;
    }
    
    
    public static PeersMessage parse(String s) throws MessageException{
        StringTokenizer token = new StringTokenizer(s);
        List<String> route = new ArrayList<String>();
        
        if (token.nextElement() != "PEERS"){
            throw new MessageException();
        }
        
        while(token.hasMoreTokens()){
            route.add(token.nextToken());
        }
    
        return new PeersMessage(route);
    }


}
