package ScalaNetworkSimulator

import scala.language.implicitConversions
import scala.language.dynamics

class NetworkSimulator {
  
  var portPointer: PortClass = null
  var devicePointer: AnyRef = null
  
  
  // The Switch object parses the commands (brackets represent optional values. Parenthesis for variables)
  // Switch name (String)
  object Switch {
    
    def name(name: String) = {
      val device = new SwitchClass(name);
      
      // Set our devicePointer reference to this class
      // Next time we call our Port object we will know to add the port to this device
      devicePointer = device
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
    
    def IPaddress(IPAddr: String) = {
      
      portPointer.IPAddr = IPAddr
    }
    
  }
  
}