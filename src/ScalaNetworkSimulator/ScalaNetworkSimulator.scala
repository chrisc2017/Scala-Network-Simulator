package ScalaNetworkSimulator

object ScalaNetworkSimulator  extends NetworkSimulator{
  
  def main( args: Array[String] ){
    

    Switch name "sw1";
      port num 3 uses "Fiber";
      port num 4 uses "Fiber";
      port num 5 uses "Fiber";
    
    Switch name "sw2"
      port num 10 uses "Fiber";
      port num 11 uses "Fiber";
      port num 12 uses "Fiber";
      
    Router name "r1";
      port num 6 uses "Fiber" IPAddress "192.168.0.1";
      port num 7 uses "Fiber" IPAddress "10.10.10.1";
      
    Router name "r2";
      port num 8 uses "Fiber" IPAddress "10.10.10.2";
      port num 9 uses "Fiber" IPAddress "44.44.44.1";
      
    PC name "Bob's PC";
      port num 1 uses "Fiber" IPAddress "192.168.0.36";
      default gateway "";
    
    PC name "Joe's PC";
      port num 2 uses "Fiber" IPAddress "192.168.0.21";
      default gateway "";
      
    PC name "Smith's PC";
      port num 13 uses "Fiber" IPAddress "44.44.44.8";
      default gateway "";
    
    PC name "Guest's PC";
      port num 14 uses "Fiber" IPAddress "44.44.44.3";
      default gateway "";
      
    Links;
      connect deviceA "sw1" portA 3 deviceB "Bob's PC" portB 1;
      connect deviceA "sw1" portA 4 deviceB "Joe's PC" portB 2;
      connect deviceA "sw1" portA 5 deviceB "r1" portB 6;
      connect deviceA "r1" portA 7 deviceB "r2" portB 8;
      connect deviceA "r2" portA 9 deviceB "sw2" portB 10;
      connect deviceA "sw2" portA 11 deviceB "Smith's PC" portB 13;
      connect deviceA "sw2" portA 12 deviceB "Guest's PC" portB 14;
    
    Config Network;
      
    print("everything worked!")
    
    
  }
 
}

//old indexes for PDU class


/*  
   * myPDU.packet(0) = source IP address
   * myPDU.packet(1) = destination IP address
   * myPDU.packet(2) = source MAC address
   * myPDU.packet(3) = destination MAC address
   * 
   * myPDU.packet(4) = data instruction keyword; ex "storeThis", "replyWith", "ARPrequest", to be sent (for ping set data to "ping", for traceroute set data to "traceroute")
   * myPDU.packet(5) = data String Key
   * myPDU.packet(6) = data String Value
   * 
   * myPDU.packet(7) = current port
   * myPDU.packet(8) = current device reference
   */


//storgeThis myfile "this is my new document"






