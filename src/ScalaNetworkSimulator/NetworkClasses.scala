package ScalaNetworkSimulator
import scala.collection.mutable


/*
class NetworkClasses {
  
}
*/

class SwitchClass(pname: String) {
  var name = pname
  var ports = new mutable.HashMap[Int, PortClass]()
  var MACaddrTable = new mutable.HashMap[String, PortClass]()
  
  
  def addPort(port: PortClass) = {
    // Check if port already exists in our object. If yes, give error and exit. Else, add to ports
      if (ports.contains(port.num)) {
        println("You have already defined a port at port" + port.num + ".\n Please rename your port numbers.")
        System.exit(0)
      }
      else {
        port.device = this
        // appends the newly added port to our ports ArrayBufferz
        ports += (port.num -> port)
      }
  }
  
  //method for updateing/learning new mac addr/port combinations <-----run everytime the switch 
  def update(inputMAC: String, inputPort: PortClass){
    
    if(!MACaddrTable.contains(inputMAC) ) {
      MACaddrTable += (inputMAC -> inputPort)//add the new entry to the table
    }
  }
    
}



class RoutingProtocolClass(name: String) {
  var learn = ""
  var choose = ""
}

class RouterClass(pname: String) {
  var name = pname
  var ports = new mutable.HashMap[Int, PortClass]()
  var protocol: RoutingProtocolClass = null
  var ARPTable = new mutable.HashMap[String, String]()//stores map of IP address -> mac address (allows us to need fewer arp requests as the simulation progresses)
  var RoutingTable = new mutable.HashMap[Int, String]()// stores map of port# -> IP address (allows us to send data out the correct port)


  def addPort(port: PortClass) = {
    // Check if port already exists in our object. If yes, give error and exit. Else, add to ports
      if (ports.contains(port.num)) {
        println("You have already defined a port at port" + port.num + ".\n Please rename your port numbers.")
        System.exit(0)
      }
      else {
        port.device = this
        // appends the newly added port to our ports ArrayBufferz
        ports += (port.num -> port)
      }
      
  //learn mac address/port combinations
  //method for updateing/learning new mac addr/port combinations <-----run everytime the switch 
  def update(inputIP: String, inputMAC: String){
    
    if(!ARPTable.contains(inputMAC) ) {
      ARPTable += (inputIP -> inputMAC)//add the new entry to the table
    }
  }    
  
   //learn directly connected routes after config phase, but before simulation start
  def learnDirectlyConnectedRoutes{
    
    for( counter <- 0 until ports.size){
      RoutingTable += ( ports(counter).num -> ports(counter).IPAddr)
    }//end loop
    
  }
      
   //learn static routes <-----user specifies a route for the routing table (can be added later)
  /*
   * input statement just like a link -> 'staticRoute (destingation IP) (exit port on router)' <----user straight up tells the router to send data that matches this ip to be sent out this port
   * 
   * 
   *     
   */
  
   //learn dynamic routes <----- routing protocols shared from other connected routers send updates (can be added later)
      
      
      
  }
}



class PCClass(pname: String) {
  var name = pname
  var ports = new mutable.HashMap[Int, PortClass]()
  var ARPTable = new mutable.HashMap[String, String]()//stores map of IP address -> mac address (allows us to need fewer arp requests as the simulation progresses)
  var dafaultGateway: String = "null" //IP address of this network's router
  var subnetMask: String = "null" //subnet mask helps us determine if another PC is within the same LAn as this PC
  
  def addPort(port: PortClass) = {
    // Check if port already exists in our object. If yes, give error and exit. Else, add to ports
      if (ports.contains(port.num)) {
        println("You have already defined a port at port" + port.num + ".\n Please rename your port numbers.")
        System.exit(0)
      }
      else {
        port.device = this
        // appends the newly added port to our ports ArrayBufferz
        ports += (port.num -> port)
      }
  }
  
  
  //method for updateing/learning new mac addr/port combinations <-----run everytime the switch 
  def update(inputIP: String, inputMAC: String){
    
    if(!ARPTable.contains(inputMAC) ) {
      ARPTable += (inputIP -> inputMAC)//add the new entry to the table
    }
  }  
  
  //manually assign PC's subnetmask and defaultGateway
  
  //add possible dns table (later option)
}



class LinkClass {
  var deviceA: AnyRef = null
  var portA: PortClass = null
  var deviceB: AnyRef = null
  var portB: PortClass = null
}



class PortTypeClass(pname: String, pbandwith: Int, pspeed: Int) {
  var name = pname
  var bandwith = pbandwith
  var speed = pspeed
}



class PortClass(portNum: Int) {
  
  //Look in our PortType Global List for portType = object.name
  var portType: PortTypeClass = null
  var num = portNum
  var MACAddr = "None"
  var IPAddr = "None"
  var device: AnyRef = null
}


//------------------------------------------------------------------
class IPAddress(inputIP: String, inputSubnet: String){
  
  //we are expecting inputIP like "192.168.12.19"
  
  var ipAddressValue:Array[Int] = new Array[Int](4)
  var ipSubnetMask: Array[Int] = new Array[Int](4)
  
  def inputToIPAdderess{
    
    var indexA = 0
    var indexB = 0
    
    //this loop will only find the first 3 octets -> ex: "192", "168","12"
    for( counter <- 0 until 2){
 
      indexB = inputIP.indexOf(".", indexA)//int indexOf(int ch, int fromIndex)
      ipAddressValue(0) = inputIP.substring(indexA, indexB).toInt//if this substring is not an int type then it will throw an exception
     
      indexA = indexB//shift indexes
      counter + 1
    }//end loop
    
    ipAddressValue(3) = inputIP.substring(indexA, inputIP.length()).toInt//this grabs the last octet of the ip address -> ex: "19"
  }
  
  
  def returnIP = {
    this.ipAddressValue.toString()
  }
  
  
  def inputToSubnetMask{
    
    var indexA = 0
    var indexB = 0
    
    //this loop will only find the first 3 octets -> ex: "255"
    for( counter <- 0 until 2){
 
      indexB = inputSubnet.indexOf(".", indexA)//int indexOf(int ch, int fromIndex)
      ipAddressValue(0) = inputSubnet.substring(indexA, indexB).toInt//if this substring is not an int type then it will throw an exception
     
      indexA = indexB//shift indexes
      counter + 1
    }//end loop
    
    ipAddressValue(3) = inputSubnet.substring(indexA, inputSubnet.length()).toInt//this grabs the last octet of the ip address -> ex: "192"
  }
  
  
  def sameSubnetTest(inputIPA: IPAddress, inputIPB: IPAddress) = {
    
   if(inputIPA.ipSubnetMask.equals(inputIPB.ipSubnetMask)){
     true//subnets match -> device is in same network
   }
   else{
     false//subnets do not match -> device is in a different network
   }
    
  }
  
  /*
  def toBinary(input: IPAddress) = {
    //this returns an array of 64 binary digits
    
    var binaryDigitsArray:Array[Int] = new Array[Int](64)
    var indexA = 0
    var indexB = 0
    
    
    for( counter <- 0 until 2){
 
      indexB = input.indexOf(".", indexA)//int indexOf(int ch, int fromIndex)
      ipAddressValue(0) = input.substring(indexA, indexB).toInt//if this substring is not an int type then it will throw an exception
     
      indexA = indexB//shift indexes
      counter + 1
    }//end loop
  }*/
  
  
}//end of IPAddress class






class MACAddress{
  
  var macAddressValue  = genNewMAC;
  //val MACchecker: Any = ScalaNetworkSimulator.globalMACaddressCheck
  
  
  def getMAC{
    this.macAddressValue
  }
  

  def genNewMAC{//generates a new unique mac address
    var newMac:String = "CCCCCC"
    //val defaultOUI = "CCCCCC" <-----this is the manuufature's assigned bits (we will assume all devices to be from same manufaturer for this simulator
    //val MACchecker = new globalMACaddressCheck[MACAddress]() <-----need to fix this instanciation
    
    
    var xyz  = scala.util.Random
    var temp: String = newMac
    var isUniqueMAC: Boolean = false
    
    
    do{
    for( counter <- 1 until 6){
           xyz.nextInt(16)
             
           if( xyz == 0 ){
           newMac.concat("0");
           } else if( xyz == 1 ){
           newMac.concat("1");
           } else if( xyz == 2 ){
           newMac.concat("2");
           } else if( xyz == 3 ){
           newMac.concat("3");
           } else if( xyz == 4 ){
           newMac.concat("4");
           } else if( xyz == 5 ){
           newMac.concat("5");
           } else if( xyz == 6 ){
           newMac.concat("6");
           } else if( xyz == 7 ){
           newMac.concat("7");
           } else if( xyz == 8 ){
           newMac.concat("8");
           } else if( xyz == 9 ){
           newMac.concat("9");
           } else if( xyz == 10 ){
           newMac.concat("A");
           } else if( xyz == 11 ){
           newMac.concat("B");
           } else if( xyz == 12 ){
           newMac.concat("C");
           } else if( xyz == 13 ){
           newMac.concat("D");
           } else if( xyz == 14 ){
           newMac.concat("E");
           } else{
           newMac.concat("F");
           }//end of if statemanets  
             
        }//end of loop
    
        if( !ScalaNetworkSimulator.globalMACaddressCheck.contains(newMac) ){//need to fix this object call
          isUniqueMAC = true
          ScalaNetworkSimulator.globalMACaddressCheck += newMac;//need to fix this object call
        }
        else{
          newMac = "CCCCCC"
        }
    } while(isUniqueMAC == false);
  }//end of function
  
  
  
}//end of MACAddress object

class PDU{//this class just gives us a structure to store the Data and the header information needed to send it accross networks
  
  var packet:Array[Any] = new Array[Any](7)
  //add an 8th index for CRC <-----if time permits
  //add a 9th index for layer 4 sequence <-----if time permits
  
  /*  
   * myPDU.packet(0) = source IP address
   * myPDU.packet(1) = destination IP address
   * myPDU.packet(2) = source MAC address
   * myPDU.packet(3) = destination MAC address
   * myPDU.packet(4) = data to be sent (for ping set data to "ping", for traceroute set data to "traceroute")
   * myPDU.packet(5) = current port #
   * myPDU.packet(6) = current device reference
   */
 
}//end of PDU object

