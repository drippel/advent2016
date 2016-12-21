

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

object Day17 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day17...")
    
    // 0 to 3
    // 0 to 3
    val input = "hijkl"
    val h = calcHash(input)
    Console.println(h)
    val mvs = calcOpen(h)
    Console.println(mvs)
    

  }
  
  class Dir( val x : Int, val y : Int, val code : Char )
  case class Up() extends Dir( 0, -1, 'U' )
  case class Down() extends Dir( 0, 1, 'D' )
  case class Right() extends Dir( 1, 0, 'R' )
  case class Left() extends Dir( -1, 0, 'L' )
  
  class Step( val x : Int, val y : Int, val hash : String, val steps : List[Step] )
  
  val OPEN = List( 'b', 'c', 'd', 'e', 'f' )
  
  def calcOpen( str : String ) : List[Dir] = {
    val open = ListBuffer[Dir]()
    
    // up, down, left, right
    if( OPEN.contains( str(0) ) ){
      open += Up()
    }
    
    if( OPEN.contains( str(1) ) ){
      open += Down()
    }
    
    if( OPEN.contains( str(2) ) ){
      open += Left()
    }
    
    if( OPEN.contains( str(3) ) ){
      open += Right()
    }

    open.toList
  }
  

  def calcHash( input : String ) : String = {
    val bytes = MessageDigest.getInstance("MD5").digest(input.getBytes)
    val md5hash1 = bytes.map("%02x".format(_)).mkString
    md5hash1
  }
}