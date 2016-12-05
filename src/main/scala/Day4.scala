

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils

object Day4 {
  
  case class Room( name : String, sectorId : String, checksum : String )
  
  case class Freq( c : Char, count : Int )
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day4...")
    
    val rooms = read( "./src/day4.txt" )
    
    Console.println(rooms.size)
    
    val validrooms = rooms.filter( isValid( _ ) )

    Console.println(validrooms.size)
    
    val ids = validrooms.map( (r:Room) => { r.sectorId.toInt })
    
    val sum = ids.foldLeft(0)( (i : Int, v : Int) => { i + v } )
    
    Console.println(sum)
    
    val r = Room( "qzmtzixmtkozyivhz", "343", "" )
    Console.println( decode( r ) )
    
    validrooms.foreach( (r : Room) => {
      
      Console.println( decode( r ) + " " + r.sectorId )
       
    })
  }
  
  def decode( room : Room ) : String = {
    
    val d = room.name.map( (c:Char) => { shift(c,room.sectorId.toInt) } ) 
    
    d
    
  }
  
  def shift( c : Char, n : Int ) : Char = {
    
    var r = c.toInt
    
    for( i <- 0 until n ){
      
      r = r + 1
      
      if( r.toChar > 'z' ){
        r = 'a'
      }
      
    }
    
    r.toChar
  }
  
  
  
  def isValid( room : Room ) : Boolean = {
    val freqs = counts( room.name )
    val sfreqs = freqs.sortWith(freqSort)
    
    for( i <- 0 until 5 ){
      
      if( room.checksum(i) != sfreqs(i).c ) {
        return false
      }
    }
    

    true
  }
  
  def freqSort( a : Freq, b : Freq ) : Boolean = {
    if( a.count > b.count ){
      true
    }
    else if( a.count == b.count ) {
      a.c < b.c
    }
    else {
      false
    }
  }
  
  def counts( code : String ) : List[Freq] = {
    
    val dcs = code.distinct
    
    val cs = for( c <- dcs ) yield  {
      val s = code.filter( _ == c )
      Freq( c, s.size )
    }
    
    cs.toList
    
  }

  def read( file : String ) : List[Room] = {
    
    val lines = IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    
    val c = lines.map( convert(_) )_
    
    c.toList

  }
  
  def convert( line : String ) : Room = {
    val ps = line.splitAt(line.indexOf('['))
    val checksum = ps._2.substring(1,ps._2.length()-1)
    
    val ps2 = ps._1.splitAt(ps._1.lastIndexOf('-'))
    val sector = ps2._2.substring(1)  
    
    val code = ps2._1.filter( _ != '-' )  

    Room( code, sector, checksum )
  }
}