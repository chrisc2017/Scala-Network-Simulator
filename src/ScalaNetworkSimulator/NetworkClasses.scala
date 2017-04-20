package ScalaNetworkSimulator

import scala.collection.mutable


/*
class NetworkClasses {
  
}
*/

class SwitchClass(name: String) {
  var ports = new mutable.ArrayBuffer()
  var MACaddrTable = new mutable.HashMap()
  
  def addPort = {
    // new port 
    //add it to our ports arrayBuffer
  }
}

class ProtocolClass(name: String) {
  
}
class RouterClass(name: String) {
  
}

class PCClass(name: String) {
  
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
  
  def setIP(address: String) = {
    this.IPAddr = address
  }
  
  def setPortType(portType: String) = {
    // set port Type to the object of portType
  }
}