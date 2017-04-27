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
}