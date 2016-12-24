

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

object Day19 {
  
  class Elf( val id : Int, var prev : Option[Elf], var next : Option[Elf] )
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day19...")
    
    // var start = init(10)
    var start = init(3004953)
    Console.println(start.id)
    
    var standing = fight(start, 1)
    Console.println(standing.id)
  }
  
  def init( size : Int ) : Elf = {
    
    val head = new Elf( 1, None, None ) 
    
    var current = head
    
    for( i <- 2 until size ){
      var next = new Elf( i, Some(current), None )
      current.next = Some(next)
      current = next
    }
    
    val last = new Elf( size, Some(current), Some(head) )
    
    current.next = Some(last)
    
    head.prev = Some(last)
    
    head
    
  }

  def fight( start: Elf, offset : Int) : Elf = {
    
    var current = start
    
    while( current.id != current.next.get.id ){
      
      // go forward offset
      for( i <- 0 until offset ){
        current = current.next.get
      }
      
      // kill current
      val prev = current.prev
      val next = current.next

      prev.get.next = next
      next.get.prev = prev 
      
      current = current.next.get
      
    }
    
    current
    
  }
  
  
  


}