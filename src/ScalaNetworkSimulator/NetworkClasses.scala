package ScalaNetworkSimulator
import scala.collection.mutable


/*
class NetworkClasses {
  
}
*/

class SwitchClass(pname: String) {
  var name = pname
  var devType = "Switch"
  var ports = new mutable.HashMap[Int, PortClass]()
  var portReceived = -1
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
  
  //cost -> how many hop
  
}

class RouterClass(pname: String) {
  var name = pname
  var devType = "Router"
  var ports = new mutable.HashMap[Int, PortClass]()
  var portReceived = -1
  var portSent: PortClass = null
  var currentWeight = 0
  var protocol: RoutingProtocolClass = null
  var ARPTable = new mutable.HashMap[String, String]()//stores map of IP address -> mac address (allows us to need fewer arp requests as the simulation progresses)
  var RoutingTable = new mutable.HashMap[String, mutable.HashMap[Int, PortClass]]()// stores map of port# -> IP address (allows us to send data out the correct port)


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
      
  //learn mac address/port combinations
  //method for updateing/learning new mac addr/port combinations <-----run everytime the switch 
  def update(inputIP: String, inputMAC: String){
    
    if(!ARPTable.contains(inputMAC)) {
      ARPTable += (inputIP -> inputMAC)//add the new entry to the table
    }
  }    
  
   //learn directly connected routes after config phase, but before simulation start
  def learnDirectlyConnectedRoutes{
    
    for (port <- ports.values){
      println("Router " + this.name + " has learned about port " + port.num + " with IP Address " + port.IPAddr)
      val map = mutable.HashMap[Int, PortClass]()
      map += ( port.portType.speed -> port)
      RoutingTable += ( port.IPAddr -> map)
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



class PCClass(pname: String) {
  var name = pname
  var devType = "PC"
  var port: PortClass = null //this is done to restrict each PC to one port
  var portReceived = -1
  var ARPTable = new mutable.HashMap[String, String]()//stores map of IP address -> mac address (allows us to need fewer arp requests as the simulation progresses)
  
  var defaultGateway: String = "null" //IP address of this network's router
  var subnetMask: String = "null" //subnet mask helps us determine if another PC is within the same LAn as this PC
  var ipAddress: String  = "null"
  
  var storage = new mutable.HashMap[String, String]()
  
  def addPort(newPort: PortClass) {
    port = newPort //assign the input Port object to this PC's port varible.
  }
  
  
  //method for updateing/learning new mac addr/port combinations <-----run everytime the switch 
  def update(inputIP: String, inputMAC: String){
    
    if(!ARPTable.contains(inputMAC) ) {
      ARPTable += (inputIP -> inputMAC)//add the new entry to the table
    }
  }  
  
  //manually assign PC's subnetmask and defaultGateway
  def assignIP(newIP: String){
    if( !ScalaNetworkSimulator.glabalIPaddressCheck.contains(newIP))
      ScalaNetworkSimulator.glabalIPaddressCheck.+=(newIP)
      ipAddress = newIP //add this new ip to the globalIP address check table
      
     
  }
  
  def assignStorage( inputKey: String, inputValue: String){
    storage += ( inputKey -> inputValue )
  }
  
  def retrieveStorage( inputKey: String) = {
    storage.get(inputKey)
  }
  
  
  def checkIncomingDestMac(incomingPDU: PDU){
   
    if(incomingPDU.packet(3) == port.MACAddr){//determines if this PDU was sent for this mac address
      
      //now determine the action to take based on the pdu.packet(4)
      if( incomingPDU.packet(4) == "storeThis" ){//store what ever was sent to this device in storage hashMap
        assignStorage( incomingPDU.packet(5).toString() , incomingPDU.packet(6).toString() )
        //sendPDU with these values:
        /*
         *myPDU.packet(4) = "Acknowledged - successful"
         *myPDU.packet(5) = "null" 
         *myPDU.packet(6) =  "null"
         */
      }
      else if(incomingPDU.packet(4) == "replyWith"){//retrieve the Value of the sent Key and then send the value back to the sending PC
        retrieveStorage( incomingPDU.packet(5).toString() )
        //sendPDU with these values:
        /*
         *myPDU.packet(4) = "Acknowledged - successful"
         *myPDU.packet(5) = "null" 
         *myPDU.packet(6) =  "null"
         */
      }
      else if(incomingPDU.packet(4) == "ARPrequest"){
        //sendPDU()
        //sendPDU with these values:
        /*
         *myPDU.packet(4) = "ARPreply" -----> this will cause the calling device to record the source MAC address
         *myPDU.packet(5) = "source IP address" 
         *myPDU.packet(6) =  "source MAC address"
         */
      }
      else if(incomingPDU.packet(4) == "ping"){
        //print out the cumulative time value *2 which is stored in the incomingPDU.packet(6) = data String Value <-----*2 because this is the round trip time from the source IP to this destination IP
        println("from (" + ipAddress + ") time= " + incomingPDU.packet(6).toString().toInt*2 +  "ms" )
      }
      else if(incomingPDU.packet(4) == "traceroute"){
        //print out the cumulative hop count + 1 <-----stored in myPDU.packet(5) for ping cmd
        print( incomingPDU.packet(4) + " ")
        
        //print out the device name then then this port's ip address
        print( incomingPDU.packet(8).asInstanceOf[PCClass].name + " (" + incomingPDU.packet(8).toString() + ") " )
      
        //print out the cumulative time value *2 which is stored in the incomingPDU.packet(6) = data String Value <-----*2 because this is the round trip time from the source IP to this destination IP
        println(" time= " + incomingPDU.packet(6).toString().toInt*2 +  "ms")
      }
        
    }
    
    
  }
  
  
  
}//end of PCClass



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
  var IPAddr: String = "None"
  var device: AnyRef = null
}


//------------------------------------------------------------------
class IPAddress(inputIP: String){
  
  //we are expecting inputIP like "192.168.12.19"
  
  var ipAddressValue: String
  
  
  def sameSubnetTest(inputIPA: IPAddress, inputIPB: IPAddress, inputSubNetMask: IPAddress): Boolean = {
    
    //var tempBits: Array[Int] = new Array[Int](8) 
    var splitA: Array[String] = inputIPA.ipAddressValue.split(".")
    var splitB: Array[String] = inputIPB.ipAddressValue.split(".")
    var splitMask: Array[String] = inputSubNetMask.ipAddressValue.split(".")
    
    var bitsA: Array[Int] = new Array[Int](8)
    var bitsB: Array[Int] = new Array[Int](8)
    var bitsMask: Array[Int] = new Array[Int](8)
    
    bitsA = toBitArray( splitA(0) )
    bitsA += toBitArray( splitA(1) )
    bitsA += toBitArray( splitA(2) )
    bitsA += toBitArray( splitA(3) )
    
    bitsB = toBitArray( splitB(0) )
    bitsB += toBitArray( splitB(1) )
    bitsB += toBitArray( splitB(2) )
    bitsB += toBitArray( splitB(3) )
    
    bitsMask = toBitArray( splitMask(0) )
    bitsMask += toBitArray( splitMask(1) )
    bitsMask += toBitArray( splitMask(2) )
    bitsMask += toBitArray( splitMask(3) )
   
    var answerA: Array[Int] = new Array[Int](32)
    var answerB: Array[Int] = new Array[Int](32)    

    
    for( index <- 0 until 31){
      if(bitsA(index) == 1 && bitsMask(index) == 1){
        answerA(index) = 1
      }
      else{
        answerA(index) = 0
      }
      
      if(bitsB(index) == 1 && bitsMask(index) == 1){
        answerB(index) = 1
      }
      else{
        answerB(index) = 0
      }
    }
    
    if( answerA.toString() == answerB.toString() )
      return true
    else
      return false
    
   
  }//end of sameSubnetTest
  
  
  
  def toBitArray(inputString: String): Array[Int] = {
    
    var bitArray: Array[Int] = new Array[Int](8)
    var num: Int = inputString.toInt
    
    if( (num -128) >= 0 ){ //128
      bitArray(0) = 1
    }
    else{
      bitArray(0) = 0
    }
    
    if( (num - 64) >= 0 ){ //64
      bitArray(1) = 1
    }
    else{
      bitArray(1) = 0
    }
    
    if( (num - 32) >= 0 ){ //32
      bitArray(2) = 1
    }
    else{
      bitArray(2) = 0
    }
    
    if( (num - 16) >= 0 ){ //16
      bitArray(3) = 1
    }
    else{
      bitArray(3) = 0
    }
    
    if( (num -8) >= 0 ){ //8
      bitArray(4) = 1
    }
    else{
      bitArray(4) = 0
    }
    
    if( (num - 4) >= 0 ){ //4
      bitArray(5) = 1
    }
    else{
      bitArray(5) = 0
    }
    
    if( (num - 2) >= 0 ){ //2
      bitArray(6) = 1
    }
    else{
      bitArray(6) = 0
    }
    
    if( (num - 1) >= 0 ){ //1
      bitArray(7) = 1
    }
    else{
      bitArray(7) = 0
    }
    
    
    
  }
  
  
  
  /*
  var ipAddressValue:Array[Int] = new Array[Int](4)
  var ipSubnetMask: Array[Int] = new Array[Int](4)
  
  def inputToIPAdderess(input: String){
    
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
  
  
  def inputToSubnetMask(input: String){
    
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
  */
  
}//end of IPAddress class






class MACAddress{
  
  var macAddressValue: String = ""
  
  

  def generateNewMAC{//generates a new unique mac address
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
          ScalaNetworkSimulator.globalMACaddressCheck += newMac;
        }
        else{
          newMac = "CCCCCC"
        }
    } while(isUniqueMAC == false);
  }//end of function
  
  
  
  
  
  
  
}//end of MACAddress object








class PDU{//this class just gives us a structure to store the Data and the header information needed to send it accross networks
  
  var packet:Array[Any] = new Array[Any](9)
  
  
  
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
 
}//end of PDU object

