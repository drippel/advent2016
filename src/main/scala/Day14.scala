

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

object Day14 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day14...")
    
    val keys = solve( "jlmsuwbz" )
    //val keys = solve( "abc" )
    
    Console.println(keys)
    Console.println(keys.size)
  }
  
  def solve( salt : String ) : List[String] = {
    
    val hashes = ListBuffer[String]()
    val keys = ListBuffer[String]()
    
    var pos = 0
    
    
    while( keys.size < 64 ){
    
      // gen keys until find a candidate - triples
      
      val set = findNextCandidate( salt, pos )
      
      // Console.println( set.last )

      // gen the next 1000 hashes
      val batch = genbatch( salt, pos + set.size + 1 )
      // Console.println( batch.size )
      
      val c = triple(set.last)
      
      // search for quint in the range
      if( batch.exists( quint( _, c ) ) ){
        keys += set.last
        Console.println( keys.size ) 
        Console.println( pos + set.size ) 
      }
      
      pos = pos + set.size
    
    }
    
    keys.toList
  }
  
  def quint( s : String, c : Char ) : Boolean = {
    
    for( i <- 0 to s.size - 5 ){
      
      if( s(i) == c  
          && s(i) == s(i+1) 
          && s(i) == s(i+2) 
          && s(i) == s(i+3)
          && s(i) == s(i+4) ){
        return true
      }
      
    }
    
    false
    
  }
  
  def genbatch( salt : String, start : Int ) : List[String] = {
    
    var hashes = ListBuffer[String]()
    
    for( i <- 1 to 1000 ){
      hashes += calcHash( salt + (start + i) )
    }
    
    hashes.toList
    
  }
  
  
  def findNextCandidate( salt : String, start : Int ) : List[String] = {
    
    val hashes = ListBuffer[String]()
    
    var idx = start
    
    var found = false
    
    while( !found ){
    
      val hash = calcHash(salt + idx)
      
      hashes += hash
      
      if( isCandidate(hash) ){
        found = true
      }
      
      idx = idx + 1

    }

    hashes.toList    
    
  }
  
  def triple( hash : String ) : Char = {
    
    for( i <- 0 to hash.size - 3 ){
      
      if( hash(i) == hash(i+1) && hash(i) == hash(i+2) ){
        return hash(i)
      }
      
    }
    
    '-' 
  }

  def isCandidate( hash : String ) : Boolean = {
    
    for( i <- 0 to hash.size - 3 ){
      
      if( hash(i) == hash(i+1) && hash(i) == hash(i+2) ){
        return true 
      }
      
    }
    
    false
  }
    
  def calcHash( input : String ) : String = {
    val bytes = MessageDigest.getInstance("MD5").digest(input.getBytes)
    val md5hash1 = bytes.map("%02x".format(_)).mkString
    md5hash1
  }
}