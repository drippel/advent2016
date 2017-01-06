

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Day25 {
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day25...")
    
    val lines = read( "./src/day25.txt" )
    
    val instrs = lines.map( convert( _ ) )

    // instrs.foreach( Console.println(_) ) 
    // run( instrs )
    // Console.println( registers )
    
  }
  
  def run( is : List[Instruction] ) = {
    
    val instr = ListBuffer[Instruction]()
    instr ++= is

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
          
          c.y match {
            case Left(i) => {
              // skip
            }
            case Right(r) => {
              registers.put(r(0),i)
            }
            
          }
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

          val d = j.y match {
            case Left(l) => {l}
            case Right(r) => { registers.get(r(0)).get }
          }

          pos = if( i != 0 ) { pos + d }
          else { pos + 1 }

        }
        case t : Tgl => {
          
          val p = pos + registers.get(t.x(0)).get
          
          if( p < instr.size ){
            val newinstr = toggle( instr(p) ) 
            instr(p) = newinstr
          }
          
          pos = pos + 1
          
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
  
  def toggle( instr : Instruction ) : Instruction = {
    
    instr match {
     case i : Inc => { Dec(i.x) } 
     case d : Dec => { Inc(d.x) }
     case t : Tgl => { Inc(t.x) }
     case c : Cpy => { Jnz( c.x, c.y ) }
     case j : Jnz => { Cpy( j.x, j.y ) }
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
        Cpy( x, Right(parts(2)) )
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
        val y = if( isNumber( parts(2) ) ){
          Left(parts(2).toInt)
        }
        else {
          Right(parts(2))
        }
        Jnz( x, y )
      }
      case "tgl" => {
        Tgl(parts(1))
      }
      case _ => { throw new IllegalArgumentException( line ) }
    }
  }
  
  def isNumber( s : String ) : Boolean = {
    s(0) <= 57 
  }
  
  val registers = HashMap[Char,Int]( ('a'-> 12), ( 'b' -> 0 ), ( 'c' -> 0), ( 'd' -> 0 ) )
  
  class Instruction()
  case class Cpy( x : Either[Int,String], y : Either[Int,String] ) extends Instruction
  case class Inc( x : String ) extends Instruction
  case class Dec( x : String ) extends Instruction
  case class Jnz( x : Either[Int,String], y : Either[Int,String] ) extends Instruction
  case class Tgl( x : String ) extends Instruction
  
  
}