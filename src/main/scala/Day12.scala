

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Day12 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day12...")
    
    val lines = read( "./src/day12.txt" )
    
    lines.foreach( Console.println(_) ) 
    
    val instrs = lines.map( convert( _ ) )
    
    run( instrs )
    
    Console.println( registers )
    
  }
  
  def run( instr : List[Instruction] ) = {
    
    var pos = 0
    var count = 0
    
    while( pos < instr.size ){
      
      // Console.println( instr(pos) ) 
      // Console.println( "pre:"+ registers ) 

      instr(pos) match {
        case c : Cpy => {
          val i = c.x match {
            case Left(i) => {i}
            case Right(r) => { registers.get(r(0)).get }
          }
          registers.put(c.y(0),i)
          pos = pos + 1
        }
        case i : Inc => {
          val r = registers.get(i.x(0))
          registers.put(i.x(0), r.get + 1 )
          pos = pos + 1
        }
        case d : Dec => {
          val r = registers.get(d.x(0))
          registers.put(d.x(0), r.get - 1 )
          pos = pos + 1
        }
        case j : Jnz => {

          val i = j.x match {
            case Left(l) => {l}
            case Right(r) => { registers.get(r(0)).get }
          }

          pos = if( i != 0 ) { pos + j.y }
          else { pos + 1 }

        }
        case _ => { throw new IllegalStateException( "bad instruction") }
      }
      
      // Console.println( "pst:"+ registers ) 
      
      count = count + 1
      
      if( count % 10000 == 0 ){
        Console.print(".")
      }
      
    }
    
  }
  
  def read( file : String ) : List[String] = {
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    l.toList
  }
  
  def convert( line : String ) : Instruction = {
    val parts = StringUtils.split(line)
    
    parts(0) match {
      case "cpy" => { 
        val x = if( isNumber( parts(1) ) ){
          Left(parts(1).toInt)
        }
        else {
          Right(parts(1))
        }
        Cpy( x, parts(2) )
      }
      case "inc" => { Inc( parts(1) ) }
      case "dec" => { Dec( parts(1) ) }
      case "jnz" => { 
        val x = if( isNumber( parts(1) ) ){
          Left(parts(1).toInt)
        }
        else {
          Right(parts(1))
        }
        Jnz( x, parts(2).toInt )
      }
      case _ => { throw new IllegalArgumentException( line ) }
    }
  }
  
  def isNumber( s : String ) : Boolean = {
    s(0) <= 57 
  }
  
  val registers = HashMap[Char,Int]( ('a'-> 0), ( 'b' -> 0 ), ( 'c' -> 1), ( 'd' -> 0 ) )
  
  class Instruction()
  case class Cpy( x : Either[Int,String], y : String ) extends Instruction
  case class Inc( x : String ) extends Instruction
  case class Dec( x : String ) extends Instruction
  case class Jnz( x : Either[Int,String], y : Int ) extends Instruction
  
  
}