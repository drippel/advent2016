

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Day20 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day20...")
    
    val lines = read( "./src/day20.txt" )
    

    val ps = lines.map( parse( _ ) )
    
    val lowest = ps.sortWith(lower)
    val highest = ps.sortWith(higher)
    
    Console.println( lowest.head )
    Console.println( highest.head )
    
    var lo = 0L
    
    for( p <- lowest ){
      
      if( lo < p._1 ){
        Console.println( "done:?" + lo )
      }
      else if( lo <= p._1 ){
        
        if( lo > p._2 ){
          // leave it
        }
        else {
          lo = p._2 + 1L
        }
        
      }
      else if( lo > p._1 ) {
        
        lo = p._2 + 1L
        
      }
      
    }
    
    Console.println(lo)
    
    
  }
  
  def lower( p1 : (Long,Long), p2 : (Long,Long) ) : Boolean = {
    if( p1._1 < p2._1 ){
      true
    }
    else {
      p1._2 < p2._2
    }
  }

  def higher( p1 : (Long,Long), p2 : (Long,Long) ) : Boolean = {
    if( p1._2 > p2._2 ){
      true
    }
    else {
      p1._1 > p2._1
    }
  }
  
  def read( file : String ) : List[String] = {
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    l.toList
  }
  
  def parse( line : String ) : (Long,Long) = {

    val parts = line.split("-")
    (parts(0).toLong,parts(1).toLong)
  }
  
}