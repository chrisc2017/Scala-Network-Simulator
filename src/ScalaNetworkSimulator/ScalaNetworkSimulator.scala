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