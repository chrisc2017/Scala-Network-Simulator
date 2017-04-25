package ScalaNetworkSimulator

import scala.collection.mutable


/*
class NetworkClasses {
  
}
*/

class SwitchClass(name: String) {
  
  var ports = new mutable.ArrayBuffer[PortClass]()
  var MACaddrTable = new mutable.HashMap()
  
  def addPort(port: PortClass) = {
    
    // appends the newly added port to our ports ArrayBufferz
    ports += port
  }
}

class RoutingProtocolClass(name: String) {
  var learn = ""
  var choose = ""
}

class RouterClass(name: String) {
  
  var ports = new mutable.ArrayBuffer[PortClass]()
  var protocol: RoutingProtocolClass = null
  
  def addPort(port: PortClass) = {
    
    // appends the newly added port to our ports ArrayBufferz
    ports += port
  }
}

class PCClass(name: String) {
  
  var ports = new mutable.ArrayBuffer[PortClass]()
  
  def addPort(port: PortClass) = {
    
    // appends the newly added port to our ports ArrayBufferz
    ports += port
  }
}

class LinkClass {
  
}

class PortTypeClass(name: String, bandwith: Int, speed: Int) {
  
}

class PortClass(portNum: Int) {
  
  //Look in our PortType Global List for portType = object.name
  var portType: PortTypeClass = null
  
  var MACAddr = "None"
  var IPAddr = "None"
}