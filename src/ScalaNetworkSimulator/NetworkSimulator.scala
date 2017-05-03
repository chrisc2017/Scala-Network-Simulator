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
  var routerRef: RouterClass = null
  
  
  
  // This table holds all the devices (Swithces,using their name as the key 
  var globalDeviceTable: mutable.HashMap[String, AnyRef] = new mutable.HashMap[String, AnyRef]()
  var globalProtocolTable: mutable.HashMap[String, RoutingProtocolClass] = new mutable.HashMap[String, RoutingProtocolClass]()
  var globalLinksTable: mutable.HashMap[PortClass, PortClass] = new mutable.HashMap[PortClass, PortClass]()
  var globalMACaddressCheck: mutable.HashSet[String] = new mutable.HashSet[String]()
  var glabalIPaddressCheck: mutable.HashSet[String] = new mutable.HashSet[String]()
  var globalRouterList: mutable.ArrayBuffer[RouterClass] = new mutable.ArrayBuffer[RouterClass]()
  var globalPortTypeTable: mutable.HashMap[String, PortTypeClass] = new mutable.HashMap[String, PortTypeClass]()
  
  globalPortTypeTable += ("Fiber" -> new PortTypeClass("Fiber", 100, 100))
  globalPortTypeTable += ("Ethernet" -> new PortTypeClass("Ethernet", 10, 10))
  
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
        globalRouterList += device
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
      port.device = deviceRef
      // sets portRef to current PortClass we created
      portRef = port
      
      return this
    }
    
    def uses(portType: String): this.type = {
      //All we have to do is set the PortType but we need to instantiate Fiber, Ethernet, etc first.
      portRef.portType = globalPortTypeTable.get(portType).get
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
  
  object Default {
    
    def Gateway(IPAddr: String) = {
      if (deviceRef.isInstanceOf[PCClass]) {
        deviceRef.asInstanceOf[PCClass].defaultGateway = IPAddr
      }
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
      
      if (linkRef.deviceA.isInstanceOf[SwitchClass]) {
        linkRef.portA = linkRef.deviceA.asInstanceOf[SwitchClass].ports.getOrElse(num, sys.error("Device A does not have port number " + num.toString + "."))
      }
      else if  (linkRef.deviceA.isInstanceOf[RouterClass]) {
        linkRef.portA = linkRef.deviceA.asInstanceOf[RouterClass].ports.getOrElse(num, sys.error("Device A does not have port number " + num.toString + "."))
      }
      else {
        if (linkRef.deviceA.asInstanceOf[PCClass].port.num != num) {
          println("Device A does not have port number " + num.toString + ".")
          sys.exit(1)
        }
        linkRef.portA = linkRef.deviceA.asInstanceOf[PCClass].port
      }
      return this
    }
    
    def deviceB(name: String): this.type = {
      linkRef.deviceB = globalDeviceTable.getOrElse(name, sys.error("Device B (named: " + name + ") does not exist. Please change the name of deviceA"))
      return this
    }
    
    def portB(num: Int): this.type = {
      
      if (linkRef.deviceB.isInstanceOf[SwitchClass]) {
        linkRef.portB = linkRef.deviceB.asInstanceOf[SwitchClass].ports.getOrElse(num, sys.error("Device A does not have port number " + num.toString + "."))
      }
      else if  (linkRef.deviceB.isInstanceOf[RouterClass]) {
        linkRef.portB = linkRef.deviceB.asInstanceOf[RouterClass].ports.getOrElse(num, sys.error("Device A does not have port number " + num.toString + "."))
      }
      else {
        if (linkRef.deviceB.asInstanceOf[PCClass].port.num != num) {
          println("Device A does not have port number " + num.toString + ".")
          sys.exit(1)
        }
        linkRef.portB = linkRef.deviceB.asInstanceOf[PCClass].port
      }
      
      if (linkRef.portA.portType != linkRef.portB.portType) {
        println("You cannot connect two ports that are different types. Please modify your configuration.")
          sys.exit(1)
      }
      
      globalLinksTable += (linkRef.portA -> linkRef.portB)
      globalLinksTable += (linkRef.portB -> linkRef.portA)
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
   * myPDU.packet(5) = current port 
   * myPDU.packet(6) = current device reference
   */
  
  
  
  //this method will find matching destination mac address for the known destination IP address
  //this request can be sent from Routers or PC's
  def requestARP(pdu: PDU){
    
    if( pdu.packet(6).isInstanceOf[SwitchClass] ){//if the current device the packet is on is a switch
  
      for(counter <- 1 to pdu.packet(6).asInstanceOf[SwitchClass].ports.size ){
        
        if(pdu.packet(6).asInstanceOf[SwitchClass].ports.get(counter) != pdu.packet(5).asInstanceOf[PortClass] ){
          
          //update PDU(5) and PDU(6) to the port# and device ref of the port on the opposite side of the Link
          pdu.packet(5) = globalLinksTable.get( pdu.packet(6).asInstanceOf[SwitchClass].ports.get(counter).get )//set next port on link as the current port
          pdu.packet(6) = globalLinksTable.get( pdu.packet(6).asInstanceOf[SwitchClass].ports.get(counter).get ).get.device //set next port's device as the current device
          println("forwarding ARP request out of port " + counter.toString())
          requestARP(pdu)//causes recursive call to flood all switch ports with an ARP request
        }
        //do nothing if the pdu(5) == the counter # <---- we dont want to forward ARPrequest back to the devices that sent the original ARPrequest
      }//end of loop
    
    }//end of dealing with switches
    
    
    //if we get here in the code then we have either reached a PC or a router
    //check if this PC's port has the IP address of the packet's destination IP
    if( pdu.packet(5).asInstanceOf[PortClass].IPAddr == pdu.packet(1) ){
          println(pdu.packet(6).asInstanceOf[PCClass].name + " successfully received the ARP request. Sending reply to "  )
    }
    else{
      println("Dropping the packet because the destination IP address does not match its IP address")
    }
    
    
    
  }//end of requestARP
  
  
  
  /*
  //sudo code for sending PDU accross the network <----- will rewrite as recursive method
  def sendPDU( pdu: PDU){
    
    if (pdu.packet(3) == "null"){
      requestARP(pdu) 
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
    
    
  }//end sendPDU*/
  
  
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
  object Config{
    
    var s: String = ""
    var input: String = ""
    var currentDevice: AnyRef = ""
    
    /*commands the user will have available:
    *
    * 1. exit -> this ends the cli and exits the simulation
    * 
    * 2. changeDevice <device name> -> this changes the current device referenced from one device to another.  
    * 
    * 3. send <IP address> <command: storgeThis | replyWith> <String of char>-> sends a Protocol Data Unit across the network to another IP. If the correct PC device IP receives the data that device prints the data to the CLI. Upon failure -> print device unreachable.
    * 
    * 4. ping <IP> -> this will send a PDU to the specified IP (any device's IP) but it will not print its path as it goes. 
    * It will only print "xx bytes from <destination IP>: time= <number of seconds the links specify added together> ms".
    * Upon failure print "device not reachable"
    *
    * 5. traceroute <IP> -> this acts just like ping, but it will only print "<counter of hop> <device name> <IP of port> time=<cummulative time in seconds that it took to get here based on link times> ms"
    * when it hits a port that has an IP address. Upon failure print "device unreachable"
    * 
    * 6. inspect [mactable | arptable | routingtable]	 -> this allows the user to see the device's specified table entries. It the table does not exits then print "Table does not exits on this type of device."
    * If table does exist this command will print out the entries to the command line.
    * 
    * Upon parsing failure of any of these commands -> print "Error: Command spelling or incorrect amount of arguments. Please see command help"
    * 
    * 7. Help just prints to the CLI a list of the command prototypes and a simple explination
    * 
    * */
    
    // Entry point to our simulation
    def Network() = {
      runCLI
    }
    
    def routerLearn(port: PortClass){
      // Gets the port from the opposite end of our port
      var oppPort: PortClass = globalLinksTable.get(port).get
      if (oppPort.device.isInstanceOf[SwitchClass]) {
        
        routerRef.currentWeight += port.portType.speed
        
        var device = oppPort.device.asInstanceOf[SwitchClass]
        device.portReceived = oppPort.num
        println(device.devType + " " + device.name + " received a packet at port " + oppPort.num +". Switches just need to forward the packets. Sending packet out of the rest of it's ports.")
        for (port <- device.ports.values) {
          if (port.num != device.portReceived) {
            
            routerLearn(port)
          }
        }
      }
      else if (oppPort.device.isInstanceOf[RouterClass]) {
        var device = oppPort.device.asInstanceOf[RouterClass]
        device.portReceived = oppPort.num
        println(device.devType + " " + device.name + " received a packet at port " + oppPort.num +". Router will send it's information it currently has.")
        for (route <- device.RoutingTable) {
          if (!routerRef.RoutingTable.contains(route._1)) {
            val map = route._2
            val weight = routerRef.currentWeight + oppPort.portType.speed
            println("Added IP Address " + route._1 + " to routing table with weight " + weight + " through port " + routerRef.portSent.num)
            map += (weight -> routerRef.portSent)
            routerRef.RoutingTable.put(route._1, map)
          }
        }
        
        for (port <- device.ports.values) {
          if (port.num != device.portReceived) {
            routerLearn(port)
          }
        }
      }
      else {
        var device = oppPort.device.asInstanceOf[PCClass]
        device.portReceived = oppPort.num
        println(device.devType + " " + device.name + " received a packet at port " + oppPort.num +". PCs will drop the packets because they provide no information.")
      }
    }
    //this runs the CLI -> this method is only called by the main method in ScalaNetworkSimulator
    def runCLI{
      //user must select a device to start with:
      var deviceRef: AnyRef = null
      var splitStringArray:Array[String] = new Array[String](4)
      // Learning phase
      // This is when router's will try to learn information about the device in the network.
      println("System booted up. Devices are powering on...")
      for (router <- globalRouterList) {
        println("Router " + router.name + " booted up.") 
        router.learnDirectlyConnectedRoutes
      }
      
      for (router <- globalRouterList) {
        println("Router " + router.name + " has initiated learning protocol.")
        
        routerRef = router
        
        println("\n\nLearning phase for router " + router.name + " beginning.")
        //Thread.sleep(500) maybe put a thread.sleep after each print statement to slow down runtime?
        println("\n\nSending packets out of all ports\n\n")
        for (port <- router.ports.values) {
          println("\nSending packet out of port " + port.num.toString + ".")
          router.portSent = port
          router.currentWeight = 0
          routerLearn(port) 
        }
      }
      
      println("Welcome to Scala Network Simulator. Please choose a device to get started or type help.")
      
      /*
      while( s != "exit" ){
           print("> ")
           s = readLine(input)//provides the user with a prompt
           splitStringArray = s.split(" ") //think about error checking the number of arguments later
           
           if(splitStringArray(0) == "ping" ){
             config.ping( splitStringArray(1) ) //send command will call sendPDU
           }
           else if(splitStringArray(0) == "traceroute" ){
             config.traceroute( splitStringArray(2)) //send command will call sendPDU
           }
           else if(splitStringArray(0) == "send" ){
             config.send( splitStringArray(1), splitStringArray(2), splitStringArray(3) ) //send command will call sendPDU
           }
           else if(splitStringArray(1) == "inspect" ){
             config.inspect( splitStringArray(2) )
           }
           else if(splitStringArray(1) == "changeDevice" ){
             config.changeDevice( splitStringArray(1) )
           }
           else if(splitStringArray(1) == "help" ){
             config.help //is this how we run a stand alone command with no arguments?
           }
           else if(splitStringArray(1) == "exit" ){
             s = "exit"
           }
            
        } */
      
      println("Thank you for trying the Scala Network Simulator")
    }
    
    
    /*
    def ping(inputIP: String){
      
      var myPDU = new PDU()
      myPDU.packet(0) = currentDevice.asInstanceOf[PCClass].port.IPAddr
      myPDU.packet(1) = inputIP
      myPDU.packet(2) = currentDevice.asInstanceOf[PCClass].port.MACAddr
      myPDU.packet(3) = currentDevice.asInstanceOf[PCClass].ARPTable.get(inputIP)
    
      myPDU.packet(4) = "ping"
      myPDU.packet(5) = null
      myPDU.packet(6) = null
    
      myPDU.packet(7) = currentDevice.asInstanceOf[PCClass].port
      myPDU.packet(8) = currentDevice
      
      //now that we have all the PDU fields filled out we can send the PDU accross the network.
      sendPDU(myPDU)

    }
    
    
    def traceroute(inputIP: String){
      var myPDU = new PDU()
      myPDU.packet(0) = currentDevice.asInstanceOf[PCClass].port.IPAddr
      myPDU.packet(1) = inputIP
      myPDU.packet(2) = currentDevice.asInstanceOf[PCClass].port.MACAddr
      myPDU.packet(3) = currentDevice.asInstanceOf[PCClass].ARPTable.get(inputIP)
    
      myPDU.packet(4) = "traceroute"
      myPDU.packet(5) = null
      myPDU.packet(6) = null
    
      myPDU.packet(7) = currentDevice.asInstanceOf[PCClass].port
      myPDU.packet(8) = currentDevice
      
      //now that we have all the PDU fields filled out we can send the PDU accross the network.
      sendPDU(myPDU)
    }
    
    
    def send(inputIP: String, inputCommand: String, inputData: String){
      
      var myPDU = new PDU()
      myPDU.packet(0) = currentDevice.asInstanceOf[PCClass].port.IPAddr
      myPDU.packet(1) = inputIP
      myPDU.packet(2) = currentDevice.asInstanceOf[PCClass].port.MACAddr
      myPDU.packet(3) = currentDevice.asInstanceOf[PCClass].ARPTable.get(inputIP)
    
      myPDU.packet(5) = inputData
      myPDU.packet(6) = null
    
      myPDU.packet(7) = currentDevice.asInstanceOf[PCClass].port
      myPDU.packet(8) = currentDevice
      
      if(inputCommand == "storeThis"){
        myPDU.packet(4) = "storeThis"
        sendPDU(myPDU)
      }
      else if(inputCommand == "replyWith"){
        myPDU.packet(4) = "replyWith"
        sendPDU(myPDU)
      }
      else{
        println("Mispelled command - please try again.")
      }      
    }
    
    
    def inspect(inputTable: String){
      //inspect [mactable | arptable | routingtable]
      
      
      if( currentDevice.isInstanceOf[PCCLass].getClass() && inputTable == "arptable" ){
          currentDevice.asInstanceOf[PCClass].ARPTable.toString()
      }
      else if( currentDevice.isInstanceOf[SwitchCLass].getClass() && inputTable == "mactable" ){
        currentDevice.asInstanceOf[SwitchCLass].MACaddressTable.toString()
      }
      else if( currentDevice.isInstanceOf[RouterCLass].getClass() && inputTable == "mactable" ){
        currentDevice.asInstanceOf[RouterClass].MACaddressTable.toString()
      }
      else if( currentDevice.isInstanceOf[RouterCLass].getClass() && inputTable == "routingtable" ){
        currentDevice.asInstanceOf[RouterClass].RoutingTable.toString()
      }
      
      
      
      else{
          println("Device does not exist in simulation. Please check your spelling and try again. Current device is: " + currentDevice.asInstanceOf[PCClass].name)
      }
      
    }
    
    */
    
    def changeDevice( inputName: String){
      
     
      var newDevice: AnyRef = null
 
      if( globalDeviceTable.contains(inputName) ){
        newDevice = globalDeviceTable.get(inputName)
      }
      else{
          println("Device does not exist in simulation. Please check your spelling and try again. ")
      }
      
    }
    
    
    
    //prints to CLI the available commands
    def help{
      println("------------------------available commands------------------------")
      println()
      println("exit		-> Ends the simulation")
      println()
      println("changedevice <device name>		-> Changes to another device's CLI.")
      println()
      println("send <IP address> <filename> <String of char without spaces>		-> sends data specified as a String without spaces across the network to another IP.")
      println()
      println("ping <IP>		-> Checks the availability of an IP address.")
      println()
      println("traceroute <IP>		-> Prints the path from the current device to the specified IP address.")
      println()
      println("inspect [mactable | arptable | routingtable]		-> Prints the contents of the specified table from the current device.")
    }
    
    
    
    
  }
  
  
  
  
}//end network simulator