package ScalaNetworkSimulator

import scala.language.implicitConversions
import scala.language.dynamics

class NetworkSimulator {
  
  var portPointer: PortClass = null
  var devicePointer: AnyRef = null
  
  object Switch {
    
    
    def name(name: String) = {
      val device = new SwitchClass(name);
      
      devicePointer = device
    }
    
  }
  
  object Port{
    
    // "Port using (type) (port#)"
    
    def num(portNum: Int): this.type = {
      
      val port = new PortClass(portNum)
      
     
      // Add PortClass object to whatever Device we created (Switch,Router,PC)
      devicePointer.getClass().getMethod("addPort", devicePointer.getClass()).invoke(devicePointer)
      
      
      // sets port Pointer to current PortClass we created
      portPointer = port
      
      return this
    }
    def uses(portType: String): this.type = {
      //portPointer.portType = classOf(portType)
      
      return this
    }
    
    def IPaddress(IPAddr: String) = {
      
      portPointer.IPAddr = IPAddr
    }
    
  }
  
}