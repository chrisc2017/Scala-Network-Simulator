package ScalaNetworkSimulator

import scala.language.implicitConversions
import scala.collection.mutable

class NetworkSimulator {
  
  // These are references we will keep in order to keep track of what devices we are setting up
  var portPointer: PortClass = null
  var devicePointer: AnyRef = null
  var protocolPointer: RoutingProtocolClass = null
  
  // This table holds all the devices (Swithces,using their name as the key 
  var globalDeviceTable: mutable.HashMap[String, AnyRef] = new mutable.HashMap[String, AnyRef]()
  var globalProtocolTable: mutable.HashMap[String, RoutingProtocolClass] = new mutable.HashMap[String, RoutingProtocolClass]()
  
  // var Fiber: PortTypeClass = PortTypeClass()
  // add it to GlobalPortType hashmap with the name as the key
  
  // The RoutingProtocol object parses the commands (brackets represent optional values. Parenthesis for variables)
  // RoutingProtocol name (String)
  object RoutingProtocol {
    
    def name(name: String) = {
      val protocol = new RoutingProtocolClass(name)
      // Set our protocolPointerPointer reference to this class
      // Next time we call our Port object we will know to add the port to this device
      protocolPointer = protocol
      
      // Check if name already exists in our globalDeviceTable. If yes, give error and exit. Else, add to table
      if (globalProtocolTable.contains(name)) {
        println("You have already defined a routing protocol named " + name + ".\n Please rename your routing protocols so they do not have conflicting names.")
        System.exit(0)
      }
      else {
        globalProtocolTable += (name -> protocolPointer)
      }
    }
  }
  
  // Helper objects for RoutingProtocol. We can access the RoutingProtocolClass object using our protocolPointer reference
  
  // The learn object will set what values the RoutingProtocolClass should use when the Router's are first ran and start learning the network
  // Learn object parses the command
  // learn by (String). The string can be only support bandwith, speed, both
  object learn {
    def by(value: String) = {
      protocolPointer.learn = value
    }
  }
  
  // The choose object will set what values the RoutingProtocolClass should use when the Router chooses which port to send data
  // choose object parses the command
  // choose by (String). The string can be only support min, max, avg
  object choose {
    def port(choose: String) = {
      protocolPointer.choose = choose
    }
  }
  
  // The Switch object parses the commands (brackets represent optional values. Parenthesis for variables)
  // Switch name (String)
  object Switch {
    
    def name(name: String) = {
      val device = new SwitchClass(name)
      // Set our devicePointer reference to this class
      // Next time we call our Port object we will know to add the port to this device
      devicePointer = device
      
      // Check if name already exists in our globalDeviceTable. If yes, give error and exit. Else, add to table
      if (globalDeviceTable.contains(name)) {
        println("You have already defined a device named " + name + ".\n Please rename your devices so they do not have conflicting names.")
        System.exit(0)
      }
      else {
        globalDeviceTable += (name -> devicePointer)
      }
    }
    
  }
  
  // The PC object parses the commands (brackets represent optional values. Parenthesis for variables)
  // PC name (String)
  object PC {
    
    def name(name: String) = {
      val device = new PCClass(name);
      
      // Set our devicePointer reference to this class
      // Next time we call our Port object we will know to add the port to this device
      devicePointer = device
      
      // Check if name already exists in our globalDeviceTable. If yes, give error and exit. Else, add to table
      if (globalDeviceTable.contains(name)) {
        println("You have already defined a device named " + name + ".\n Please rename your devices so they do not have conflicting names.")
        System.exit(0)
      }
      else {
        globalDeviceTable += (name -> devicePointer)
      }
    }
    
  }
  
  // The Router object parses the commands (brackets represent optional values. Parenthesis for variables)
  // Router name (String)
  object Router {
    
    def name(name: String) = {
      val device = new RouterClass(name);
      
      // Set our devicePointer reference to this class
      // Next time we call our Port object we will know to add the port to this device
      devicePointer = device
      
      // Check if name already exists in our globalDeviceTable. If yes, give error and exit. Else, add to table
      if (globalDeviceTable.contains(name)) {
        println("You have already defined a device named " + name + ".\n Please rename your devices so they do not have conflicting names.")
        System.exit(0)
      }
      else {
        globalDeviceTable += (name -> devicePointer)
      }
    }
    
  }
  
  // Helper objects for Router
  
  
  // The routing object will select what routing protocol the router should use
  // routing object parses the command
  // routing protocol (String). 
  object routing {
    
    def protocol(name: String) = {
      
      val router = devicePointer.asInstanceOf[RouterClass]
      router.protocol = globalProtocolTable.getOrElse(name, sys.error("You are trying to assign a router to routing protocol " + name + "which does not exist.\nPlease check your configuration file."))
    }
  }
  
  
  // The Port object parses the commands (brackets represent optional values. Parenthesis for variables)
  // Port num (Int) [uses (String)] [IPaddress (String)]
  object Port{
    
    def num(portNum: Int): this.type = {
      
      // Create new the PortClass object
      val port = new PortClass(portNum)
      
      
      // Add PortClass object to whatever Device we created (Switch,Router,PC)
      // The reason we invoke a method in this instance is because we know that SwitchClass, RouterClass
      //   and PCClass all have the method addPort.
      devicePointer.getClass().getMethod("addPort", classOf[Any]).invoke(devicePointer, port)
      
      // sets port Pointer to current PortClass we created
      portPointer = port
      
      return this
    }
    
    def uses(portType: String): this.type = {
      //All we have to do is set the PortType but we need to instantiate Fiber, Ethernet, etc first.
      //portPointer.portType = classOf(portType)
      
      return this
    }
    
    def IPAddress(IPAddr: String) = {
      /* Can do checks here to make
      if (devicePointer.getClass() == classOf[SwitchClass]) {
        print("error: you cannot assign an IP Address to a switch port")
        exit(0)
      }*/
      
      portPointer.IPAddr = IPAddr
    }
    
  }
  
}