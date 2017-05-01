package ScalaNetworkSimulator

object ScalaNetworkSimulator  extends NetworkSimulator{
  
  def main( args: Array[String] ){
    
    RoutingProtocol name "rp1";
      learn by "bandwith";
      choose port "min";
      
    Switch name "sw3";
      port num 5 uses "Fiber";
      
    Router name "r3";
      port num 8 uses "Ethernet";
      
    PC name "pc1";
      port num 9 uses "Fiber";
      
    Links;
      connect deviceA "sw3" portA 5 deviceB "pc1" portB 9;
      
    print("everything worked!")
    
    
  }
 
}



/*  
   * myPDU.packet(0) = source IP address
   * myPDU.packet(1) = destination IP address
   * myPDU.packet(2) = source MAC address
   * myPDU.packet(3) = destination MAC address
   * 
   * myPDU.packet(4) = data instruction keyword; ex "storgeThis", "replyWith", "ARPrequest", to be sent (for ping set data to "ping", for traceroute set data to "traceroute")
   * myPDU.packet(5) = data String Key
   * myPDU.packet(6) = data String Value
   * 
   * myPDU.packet(7) = current port
   * myPDU.packet(8) = current device reference
   */






