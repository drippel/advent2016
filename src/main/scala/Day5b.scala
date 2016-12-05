

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest

object Day5b {
  
  val password = initPassword()
  
  def initPassword() : ListBuffer[Option[Char]] = {
    
    val lb = new ListBuffer[Option[Char]]()
    
    for( i <- 0 until 8 ){
      
      lb += None
    }
    
    lb
    
  }
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day5...")
    
    val start = "ugkcyxxp"
    // val start = "abc"
    
    val hashes = collectHashes(start)
    
    Console.println(password.flatten)
    
    
  }
  
  def collectHashes( start : String ) : List[String] = {
    
    val hashes = ListBuffer[String]()
    
    var index = 0
    
    while( hashes.size < 8 ) {
      
      val input = start + index 
      
      val hash = calcHash(input)
      
      if( hash.startsWith("00000") ){
        if( addToHash( hash ) ){
          Console.println(input)
          hashes += hash
        }
      }
      
      if( index % 100000 == 0 ){
        Console.println(index)
      }
      
      index = index + 1
      
    }
    
    hashes.toList
    
  }
  
  def addToHash( hash : String ) : Boolean = {
    
    val pos = hash(5).toInt - '0'.toInt
    
    Console.println(pos)
    
    if( pos > 7 ){
      false
    }
    else {
    
      password(pos) match {
        case Some(c) => { false }
        case None => {
          val c = hash(6)
          password(pos) = Some(c)
          true
        }
      }
    }
    
  }
  
  def calcHash( input : String ) : String = {
    val bytes = MessageDigest.getInstance("MD5").digest(input.getBytes)
    val md5hash1 = bytes.map("%02x".format(_)).mkString
    md5hash1
  }
  
}