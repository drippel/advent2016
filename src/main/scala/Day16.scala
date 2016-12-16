

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

object Day16 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day16...")
    
    /*
    
    val t = "110010110100"
    Console.println( checksum( t, 12 ) ) 
    
    val ex1 = "10000"
    val d1 = gendata( ex1, 20 ) 
    val c1 = checksum( d1, 20 )
    Console.println( c1 )
    
    val len = 272
    val d2 = gendata( start, len )
    val c2 = checksum( d2, len )
    Console.println( c2 )
    */

    // val start = "10000".toCharArray()
    // val len = 20
    // val start = "110010110100".toCharArray()
    // val len = 12
    val start = "00101000101111010".toCharArray()
    // val len = 272
    val len = 35651584 
    val d = gendata( start, len )
    Console.println("done:"+ d.length)
    val c2 = checksum( d, len )
    print(c2)
    
    
  }
  
  def print( chars : Array[Char] ) = {
    for( i <- 0 until chars.length ){
      Console.print( chars(i) )
    }
    Console.print("\n")
  }
  
  def checksum( start : Array[Char], len : Int ) : Array[Char] = {
    
    val begin = start.slice( 0, len )
    
    var chk = begin
    
    while( chk.length() % 2 == 0 ){
      
      var next = Array.ofDim[Char](chk.length/2)
      var p = 0
      
      for( i <- 0 until chk.size by 2 ){
        
        val c = if( chk(i) == chk(i+1) ){ '1' } else { '0' }
        next(p) = c
        p = p + 1
        
      }
      
      chk = next
      
      Console.println( chk.length() )
      
    }
    
    chk
  }
  
  
  def gendata( start : Array[Char], len : Int ) : Array[Char] = {
    var data = start 
    
    while( data.length() <= len ){
      data = dragon(data)
    }
    
    data
  }
  
  def dragon( start : Array[Char] ) : Array[Char] = {
    val end = Array.ofDim[Char](start.length())
    start.copyToArray(end)
    reverse(end)
    flip(end)
    
    val combined = Array.ofDim[Char]((start.length() * 2)+1)
    
    for( i <- 0 until start.length() ){
      
      combined(i) = start(i)
      combined(i + start.length + 1) = end(i)
    
    }
    
    combined(start.length) = '0'
    
    combined
  }
  
  def reverse( start : Array[Char] ) = {
    
    for( i <- 0 until start.length / 2 ){
      val c = start(i)
      start(i) = start(start.length - 1 - i ) 
      start((start.length - 1) - i ) = c 
    }
  }
  
  def flip( start : Array[Char] ) = {
    
    for( i <- 0 until start.length ){
      if( start(i) == '0' ){
        start(i) = '1'
      }
      else{
        start(i) = '0'
      }
    }
  }
}