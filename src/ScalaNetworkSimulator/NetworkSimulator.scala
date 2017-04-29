package ScalaNetworkSimulator

import scala.language.implicitConversions
import scala.collection.mutable
import scala.io.StdIn.{readLine}//this allows us to read from the command line <-----used in the object 'config'

class NetworkSimulator {
  
  // These are references we will keep in order to keep track of what devices we are setting up
  var portRef: PortClass = null
  var deviceRef: AnyRef = null
  var protocolRef: RoutingProtocolClass = null
  var linkRef: LinkClass = null
  
  
  
  // This table holds all the devices (Swithces,using their name as the key 
  var globalDeviceTable: mutable.HashMap[String, AnyRef] = new mutable.HashMap[String, AnyRef]()
  var globalProtocolTable: mutable.HashMap[String, RoutingProtocolClass] = new mutable.HashMap[String, RoutingProtocolClass]()
  var globalLinksTable: mutable.HashMap[PortClass, PortClass] = new mutable.HashMap[PortClass, PortClass]()
  var globalMACaddressCheck: mutable.HashSet[MACAddress] = new mutable.HashSet[MACAddress]()
  
  
  // var Fiber: PortTypeClass = PortTypeClass()
  // add it to GlobalPortType hashmap with the name as the key
  
  // The RoutingProtocol object parses the commands (brackets represent optional values. Parenthesis for variables)
  // RoutingProtocol name (String)
  object RoutingProtocol {
    
    def name(name: String) = {
      val protocol = new RoutingProtocolClass(name)
      // Set our protocolRefRef reference to this class
      // Next time we call our Port object we will know to add the port to this device
      protocolRef = protocol
      
      // Check if name already exists in our globalDeviceTable. If yes, give error and exit. Else, add to table
      if (globalProtocolTable.contains(name)) {
        println("You have already defined a routing protocol named " + name + ".\n Please rename your routing protocols so they do not have conflicting names.")
        System.exit(0)
      }
      else {
        globalProtocolTable += (name -> protocolRef)
      }
    }
  }
  
  // Helper objects for RoutingProtocol. We can access the RoutingProtocolClass object using our protocolRef reference
  
  // The learn object will set what values the RoutingProtocolClass should use when the Router's are first ran and start learning the network
  // Learn object parses the command
  // learn by (String). The string can be only support bandwith, speed, both
  object learn {
    def by(value: String) = {
      protocolRef.learn = value
    }
  }
  
  // The choose object will set what values the RoutingProtocolClass should use when the Router chooses which port to send data
  // choose object parses the command
  // choose by (String). The string can be only support min, max, avg
  object choose {
    def port(choose: String) = {
      protocolRef.choose = choose
    }
  }
  

  
  
  // The Switch object parses the commands (brackets represent optional values. Parenthesis for variables)
  // Switch name (String)
  object Switch {
    
    def name(name: String) = {
      val device = new SwitchClass(name)
      // Set our deviceRef reference to this class
      // Next time we call our Port object we will know to add the port to this device
      deviceRef = device
      
      // Check if name already exists in our globalDeviceTable. If yes, give error and exit. Else, add to table
      if (globalDeviceTable.contains(name)) {
        println("You have already defined a device named " + name + ".\n Please rename your devices so they do not have conflicting names.")
        System.exit(0)
      }
      else {
        globalDeviceTable += (name -> deviceRef)
      }
    }
    
  }
  
  // The PC object parses the commands (brackets represent optional values. Parenthesis for variables)
  // PC name (String)
  object PC {
    
    def name(name: String) = {
      val device = new PCClass(name);
      
      // Set our deviceRef reference to this class
      // Next time we call our Port object we will know to add the port to this device
      deviceRef = device
      
      // Check if name already exists in our globalDeviceTable. If yes, give error and exit. Else, add to table
      if (globalDeviceTable.contains(name)) {
        println("You have already defined a device named " + name + ".\n Please rename your devices so they do not have conflicting names.")
        System.exit(0)
      }
      else {
        globalDeviceTable += (name -> deviceRef)
      }
    }
    
  }
  
  // The Router object parses the commands (brackets represent optional values. Parenthesis for variables)
  // Router name (String)
  object Router {
    
    def name(name: String) = {
      val device = new RouterClass(name);
      
      // Set our deviceRef reference to this class
      // Next time we call our Port object we will know to add the port to this device
      deviceRef = device
      
      // Check if name already exists in our globalDeviceTable. If yes, give error and exit. Else, add to table
      if (globalDeviceTable.contains(name)) {
        println("You have already defined a device named " + name + ".\n Please rename your devices so they do not have conflicting names.")
        System.exit(0)
      }
      else {
        globalDeviceTable += (name -> deviceRef)
      }
    }
    
  }
  
  // Helper objects for Router
  
  
  // The routing object will select what routing protocol the router should use
  // routing object parses the command
  // routing protocol (String). 
  object routing {
    
    def protocol(name: String) = {
      
      val router = deviceRef.asInstanceOf[RouterClass]
      router.protocol = globalProtocolTable.getOrElse(name, sys.error("You are trying to assign a router to routing protocol " + name + "which does not exist.\nPlease check your configuration file."))
    }
  }
  
  
  // The Port object parses the commands (brackets represent optional values. Parenthesis for variables)
  // port num (Int) [uses (String)] [IPaddress (String)]
  object port{
    
    def num(portNum: Int): this.type = {
      
      // Create new the PortClass object
      val port = new PortClass(portNum)
      
      
      // Add PortClass object to whatever Device we created (Switch,Router,PC)
      // The reason we invoke a method in this instance is because we know that SwitchClass, RouterClass
      //   and PCClass all have the method addPort.
      deviceRef.getClass().getMethod("addPort", classOf[PortClass]).invoke(deviceRef, port)
      
      // sets portRef to current PortClass we created
      portRef = port
      
      return this
    }
    
    def uses(portType: String): this.type = {
      //All we have to do is set the PortType but we need to instantiate Fiber, Ethernet, etc first.
      //portRef.portType = classOf(portType)
      
      return this
    }
    
    def IPAddress(IPAddr: String) = {
      /* Can do checks here to make
      if (deviceRef.getClass() == classOf[SwitchClass]) {
        print("error: you cannot assign an IP Address to a switch port")
        exit(0)
      }*/
      
      portRef.IPAddr = IPAddr
    }
    
  }
  
  // Links between devices
  // We only use this object as a syntax requirement (meaning you have to specify your Links; section in the configuration)
  object Links {
    // Don't really need to do anything here
  }
  
  // links helper methods
  
  // The connect object is what we are using to instantitate LinkClass objects
  // The connect object parses the commands (brackets represent optional values. Parenthesis for variables)
  // connect deviceA (String) portA (Int) deviceB (String) portB (Int)
  object connect {
    
    //entry point of our connect command
    def deviceA(name: String): this.type = {
      val link = new LinkClass()
      link.deviceA = globalDeviceTable.getOrElse(name, sys.error("Device A (named: " + name + ") does not exist. Please change the name of deviceA"))
      linkRef = link
      return this
    }
    
    def portA(num: Int): this.type = {
      return this
    }
    
    def deviceB(name: String): this.type = {
      return this
    }
    
    def portB(num: Int): this.type = {
      return this
    }
  }
  
  
  //sudo code for methods that move data packet (called PDU -----> "Protocol Data Unit"):
  /*
   * 
   * 
   * //------------------------------------------------------------------------------------------
   * when using any PC command: send, ping, traceroute -> read input form CLI and parse into a PDU
   * 
   * myPDU.packet(0) = source IP address
   * myPDU.packet(1) = destination IP address
   * myPDU.packet(2) = source MAC address
   * myPDU.packet(3) = destination MAC address
   * myPDU.packet(4) = data to be sent (for ping set data to "ping", for traceroute set data to "traceroute")
   * myPDU.packet(5) = current port #
   * myPDU.packet(6) = current device reference
   */
  
  
  
  //this method will find matching destination mac address for the known destination IP address
  //this request can be sent from Routers or PC's
  def requestARP(pdu: PDU){
    
    if(pdu(6).getClass().isInstanceOf[SwitchClass]){
      for(counter <- 1 to pdu(6).getClass().ports.size() ){
        
        if(pdu(6).getClass().ports.get(counter) != pdu(5) ){
          //update PDU(5) and PDU(6) to the port# and device ref of the port on the opposite side of the Link
          pdu(5) = globalLinksTable.get(pdu(6).getClass().ports.get(counter).num)
          pdu(6) = globalLinksTable.get(pdu(6).getClass().ports.get(counter).device)//will need to fix this .device soon
          println("forwarding ARP request out of port " + counter.toString())
          requestARP(pdu)//causes recursive call
        }
        //do nothing if the pdu(5) == the counter # <---- we dont want to forward ARPrequest back to the devices that sent the original ARPrequest
      }//end of loop
    }
    else {
      println("Device "+pdu(6).name + "is dropping the packet because the Dest IP address does not match its IP address)
    }
    
    
  }
  
  //sudo code for sending PDU accross the network <----- will rewrite as recursive method
  def sendPDU( this.pdu: PDU){
    
    if (pdu(3) == "null"){
      pdu(3) = sendARP(PDU) 
    }
    
    while( pdu(1) != pdu(6).getClass().port.num   ){
        
        //if the current port is on a PC
        if( pdu(6).getClass() == PCClass ){
            pdu(5) = globalLinksTable.get( pdu(6) ).port.num //get the new port number
            pdu(6) = globalLinksTable.get( pdu(6) ) //how we move to the next device
            println("sending PDU out of port " + pdu(5).toString() )
        }
        
        //if the current port is on a Switch
        if( pdu(6).getClass() == SwitchClass ){
          
          //if the incoming port # is in the table
          if( pdu(6).MACAddressTable.contains(pdu(3)) ){
              
              //get new port number
              var tempPort: PortClass = new PortClass
              tempPort.portnum = pdu(6).MACAddressTable.get( pdu(3)).portnum
              tempPort.device = pdu(6)
              
              //get next port
              pdu(5) = globalLinksTable.get( tempPort).portnum
              pdu(6) = globalLinksTable.get( tempPort).device
          }
          else{
              //flood pdu copies out of each port
              var incomingPort = pdu(5)
              
              for( counter <- 1 to pdu(6).portList.size() ){
                    
                  //only get next port 
                  if( pdu(6).portList(counter) != incomingPort ){
                      
                        //get Link's nextPort
                        var tempPort = globalLinksTable.get( pdu(6).portList(counter) )
                        pdu(5) = tempPort.portnum
                        pdu(6) = tempPort.device
                        sendPDUf( this.pdu)
                      
                }
              }//end of for
          
            
            
          }//end of else
        }//end of switch
        
        
        //if the current port is on a Router
        if( pdu(6).getClass == RouterClass ){
          
            //get exit port
            var tempPort: PortClass = new PortClass
            tempPort.portnum = pdu(6).RoutingTable.get( pdu(3) ).portnum
            tempPort.device = pdu(6)
            
            //get next port on this Link
            pdu(5) = globalLinksTable.get( tempPort ).portnum
            pdu(6) = globalLinksTable.get( tempPort ).device
        }
        else{
            println("no known route...dropping packet")
        }
        
        
        
    }
    
    
  }//end sendPDU
  
  
  //this object builds the network
  /*
   * theory: as each line of the main method is read in via the scala main method the individual objects create network
   * devices, their properties, and the links between them. 
   * 
   * This object call is written at the end of the main method code so that it launches the CLI.
   * 
   * the purpose of the config object is provide the user access to a CLI environment so that interactions with the 
   * simulated network can happen
   */
  object config{
    
    var s: String = ""
    var input: String = ""
    
    /*commands the user will have available:
    *
    * 1. exit -> this ends the cli and exits the simulation
    * 
    * 2. changeDevice <device name> -> this changes the current device referenced from one device to another.  
    * 
    * 3. send <IP address> <String of char>-> sends a Protocol Data Unit across the network to another IP. If the correct PC device IP receives the data that device prints the data to the CLI. Upon failure -> print device unreachable.
    * 
    * 4. ping <IP> -> this will send a PDU to the specified IP (any device's IP) but it will not print its path as it goes. 
    * It will only print "xx bytes from <destination IP>: time= <number of seconds the links specify added together> ms".
    * Upon failure print "device not reachable"
    *
    * 5. traceroute <IP> -> this acts just like ping, but it will only print "<counter of hop> <device name> <IP of port> time=<cummulative time in seconds that it took to get here based on link times> ms"
    * when it hits a port that has an IP address. Upon failure print "device unreachable"
    * 
    * 6. inspect ["mactable" | "arptable" | "routingtable"] <device name> -> this allows the user to see the device's specified table entries. It the table does not exits then print "Table does not exits on this type of device."
    * If table does exist this command will print out the entries to the command line.
    * 
    * Upon parsing failure of any of these commands -> print "Error: Command spelling or incorrect amount of arguments. Please see command help"
    * 
    * 7. Help just prints to the CLI a list of the command prototypes and a simple explination
    * 
    * */
    
    
    //this runs the CLI
    def runCLI{
      //user must select a device to start with:
      var deviceRef: Any = readLine("Select a device by name:" + input)
      
      
      while( s != "exit" ){
           s = readLine("Sim> " + input)//provides the user with a prompt
           s = input.toLowerCase()//grabs only the information we wanted in the first place and stores it in var s. We only need to deal with lower case for simplicity
           
        }
      
      println("Thank you for trying the Scala Simulator")
    }
    
    
    
    //prints to CLI the available commands
    def help{
      println("------------------------available commands------------------------")
      println()
      println("exit		-> Ends the simulation")
      println()
      println("changedevice <device name>		-> Changes to another device's CLI.")
      println()
      println("send <IP address> <String of char>		-> sends data specified as a String across the network to another IP.")
      println()
      println("ping <IP>		-> Checks the availability of an IP address.")
      println()
      println("traceroute <IP>		-> Prints the path from the current device to the specified IP address.")
      println()
      println("inspect [mactable | arptable | routingtable] <device name>		-> Prints the contents of the specified table from the specified device.")
    }
    
    
    
    
  }
  
  
  
  
}//end network simulator