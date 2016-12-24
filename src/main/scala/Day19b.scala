

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue
import scala.collection.mutable.HashSet

object Day19b {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day19b...")
    
    // var start = init(5)
    // var start = init(10)
    var start = init(3004953)
    // Console.println(start)
    
    var standing = fight(start)
    Console.println(standing)
  }
  
  def init( size : Int ) : ListBuffer[Int] = {
    
    val elves = new ListBuffer[Int]
    
    for( i <- 1 to size ){
      elves += i
    }
    
    elves
  }

  def fight( elves : ListBuffer[Int] ) : Int = {
    
    var d = 2
    var current = 0
    
    while( elves.size > 1 ){
      
      // calc the offset
      val offset = ( elves.size / d )
      
      val pos = (current + offset) % elves.size 

      val k = elves(pos)
      //Console.println( "s: "+ elves.size +" c:"+ elves(current) +" p:"+ pos +" k:"+ k )

      elves.remove(pos)
      
      if( pos > current ){
       current = current + 1
      }

      if( current >= elves.size ){
        current = 0
      }
      
      if( elves.size % 10000  == 0 ){
        Console.println( elves.size )
      }

    }
    
    elves.head
    
  }
  
}