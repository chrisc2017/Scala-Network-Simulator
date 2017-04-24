package ScalaNetworkSimulator

object ScalaNetworkSimulator  extends NetworkSimulator{
  
  def main( args: Array[String] ){
    
    Switch name "sw3";
      Port num 5 uses "Fiber";
      
    var a: Int = 4
    print(a.getClass())
    println("hello world")
    
    
  }
 
}