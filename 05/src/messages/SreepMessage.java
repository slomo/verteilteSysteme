package messages;

import java.util.ArrayList;
import java.util.List;
import java.util.StringTokenizer;

public class SreepMessage extends ProtocollMessage {

    MessageType type = MessageType.SREEP;
    
    List<String> route;
    List<String> peers;
    
    public SreepMessage(List<String> route, List<String> peers){
        this.route = route;
        this.peers = peers;
    }
    
    @Override
    public byte[] getData() {
        List<String> fields = new ArrayList<String>(route);
        fields.add("#");
        fields.addAll(peers);
        
        return makeMessage(type, fields);
    }

    @Override
    public int getLength() {
        return getData().length;
    }

    @Override
    public MessageType getType() {
        return type;
    }
    
    
    public static SreepMessage parse(String s) throws MessageException{
        StringTokenizer token = new StringTokenizer(s);
        List<String> route = new ArrayList<String>();
        List<String> peers = new ArrayList<String>();
        
        if (token.nextElement() != "SREEPS"){
            throw new MessageException();
        }
        
        while (token.hasMoreElements()) {
            String ele = token.nextToken();
            if(ele == "#"){
                break;
            } else {
                route.add(ele);
            }
        }
        
        while(token.hasMoreTokens()){
            peers.add(token.nextToken());
        }
    
        return new SreepMessage(route,peers);
    }

}
