package applications;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

public class Routing {
	
	public Map<String,List<String>> table;
	
	public List<String> getRoute(String node){
		return table.get(node);
	}
	
	public String getRouteBase(String node){
		return table.get(node).get(0);
	}
	
	public void updateRoutes(String node,Set<String> peers){
		
		List<String> baseRoute = table.get(node);
		
		if(baseRoute ==  null){
			System.err.println("UNKONW BASE ROUTE: " + node + " assumning local");
			baseRoute = new ArrayList<String>();
		}
		
		for(String newNode : peers){
			if(table.containsKey(newNode)){
				continue;
			} else {
				List<String> route = new ArrayList<String>(baseRoute);
				route.add(newNode);
				table.put(node,baseRoute);
			}
		}
	}
}
