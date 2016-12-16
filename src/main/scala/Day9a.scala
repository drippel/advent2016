

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Day9a {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day9...")
    
    val lines = read( "./src/day9.txt" )
    
    
    val dec = decompress( lines.head )
    Console.println(dec)
    
  }
  
  def decompress( src : String ) : Long = {
    
    var output = 0L
    
    var found = true
    var pos = 0
    
    while( found ){
      
      var next = src.indexOf( '(', pos )
      
      if( next > -1 ){
      
        // add the sub string to the output
        var front = src.substring(pos, next)
        output = output + front.length()
        
        // pull out the compress instruction
        var end = src.indexOf(')',next)
        
        val in = src.substring( (next+1), end )
        
        val parts = in.split('x')
        
        val len = parts(0).toInt
        val rep = parts(1).toInt
        
        val block = src.substring( (end+1), (end + 1 + len) )
        val dblock = decompress(block)
        
        for( i <- 0 until rep ) {
          output = output + dblock
        }
        
        pos = (end + len + 1)
        
      }
      else {
        // add the rest of the src to output
        val end = src.substring(pos)
        output = output + end.length()
        found = false
      }
      
    }

    output 
  }
  
  def read( file : String ) : List[String] = {
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    l.toList
  }
  
}