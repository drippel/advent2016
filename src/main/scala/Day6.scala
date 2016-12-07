

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Day6 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day6...")
    
    val lines = read( "./src/day6.txt" )
    
    val col1 = lines.map( _(0) )
    val col2 = lines.map( _(1) )
    val col3 = lines.map( _(2) )
    val col4 = lines.map( _(3) )
    val col5 = lines.map( _(4) )
    val col6 = lines.map( _(5) )
    val col7 = lines.map( _(6) )
    val col8 = lines.map( _(7) )

    val c1 = greatest( freqs( col1 ) ) 
    val c2 = greatest( freqs( col2 ) ) 
    val c3 = greatest( freqs( col3 ) ) 
    val c4 = greatest( freqs( col4 ) ) 
    val c5 = greatest( freqs( col5 ) ) 
    val c6 = greatest( freqs( col6 ) ) 
    val c7 = greatest( freqs( col7 ) ) 
    val c8 = greatest( freqs( col8 ) ) 
    
    Console.println( List( c1,c2,c3,c4,c5,c6,c7,c8 ) ) 
    
    
  }

  def read( file : String ) : List[String] = {
    
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    
    l.toList
    

  }
  
  def initialize() : HashMap[Char,Int] = {
    
    val cs = new HashMap[Char,Int]()
    
    for( c <- 'a' to 'z' ) {
      
      cs += (c -> 0)
      
    }
    
    cs
    
  }
  
  def freqs( chars : List[Char] ) : Map[Char,Int] = {
    
    val map = initialize()
    
    for( c <- chars ){
      map.put(c, ( map.get(c).get + 1 ))
    }
    
    map.toMap
    

  }
  
  def greatest( chars : Map[Char,Int] ) : Char = {
    
    var greatest : Char = '?'
    var count = 100
    
    for( kv <- chars ){
      
      if( kv._2 < count ){
        count = kv._2
        greatest = kv._1
      }

    }

    greatest
    
  }
}