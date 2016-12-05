

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest

object Day5 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day5...")
    
    val start = "ugkcyxxp"
    // val start = "abc"
    
    val hashes = collectHashes(start)
    
    val chars = hashes.map( _.charAt(5) )
    
    Console.println(chars.mkString)
    
    
  }
  
  def collectHashes( start : String ) : List[String] = {
    
    val hashes = ListBuffer[String]()
    
    var index = 0
    
    while( hashes.size < 8 ) {
      
      val input = start + index 
      
      val hash = calcHash(input)
      
      if( hash.startsWith("00000") ){
        Console.println(input)
        hashes += hash
      }
      
      if( index % 100000 == 0 ){
        Console.println(index)
      }
      
      index = index + 1
      
    }
    
    hashes.toList
    
  }
  
  def calcHash( input : String ) : String = {
    val bytes = MessageDigest.getInstance("MD5").digest(input.getBytes)
    val md5hash1 = bytes.map("%02x".format(_)).mkString
    md5hash1
  }
  
}