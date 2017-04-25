package ScalaNetworkSimulator

import scala.collection.mutable


/*
class NetworkClasses {
  
}
*/

class SwitchClass(pname: String) {
  var name = pname
  var ports = new mutable.HashMap[Int, PortClass]()
  var MACaddrTable = new mutable.HashMap()
  
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
}

class RoutingProtocolClass(name: String) {
  var learn = ""
  var choose = ""
}

class RouterClass(pname: String) {
  var name = pname
  var ports = new mutable.HashMap[Int, PortClass]()
  var protocol: RoutingProtocolClass = null
  
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
}

class PCClass(pname: String) {
  var name = pname
  var ports = new mutable.HashMap[Int, PortClass]()
  
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