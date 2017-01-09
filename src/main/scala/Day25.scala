

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Day25 {
  
  var registers = HashMap[Char,Int]( ('a'-> 0), ( 'b' -> 0 ), ( 'c' -> 0), ( 'd' -> 0 ) )
  
  def resetRegisters() = {
    registers = HashMap[Char,Int]( ('a'-> 0), ( 'b' -> 0 ), ( 'c' -> 0), ( 'd' -> 0 ) )
  }

  def main( args : Array[String] ) : Unit = {
    Console.println("day25...")
    
    val lines = read( "./src/day25.txt" )
    
    val instrs = lines.map( convert( _ ) )

    // instrs.foreach( Console.println(_) ) 
    // registers.put( 'a', 12 )
    debug=false
    // run( instrs )
    solve( instrs )
    // Console.println( registers )

  }
  
  var debug = false
  
  def dump() = {
    Console.println( registers )
  }
  
  def solve( is : List[Instruction] ) = {
    
    var min = 0
    
    var found = false
    while( !found ){
      
      // reset the output
      resetOut()
      
      // keset the registers
      resetRegisters()
      
      // set a
      registers.put('a', min )
      
      Console.println("Testing:"+ min )
      
      // run
      run(is)
      
      // test the output
      if( EXPECTED.equals(output) ){
        found = true
        Console.println("Found:"+ min )
      }
      
      min = min + 1
    }
  }
  
  val EXPECTED = "0101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101010101"

  def resetOut() = {
    stop = false
    output = ""
  }
  
  var stop = false
  var output = "" 
  
  def transmit( i : Int ) = {
    output = output + i
    
    if( output.length() >= 100 ){
      Console.println( "\nOutput:"+ output)
      stop = true
    }
  }
  
  def run( is : List[Instruction] ) = {
    
    val instr = ListBuffer[Instruction]()
    instr ++= is

    var pos = 0
    var count = 0
    
    if( debug ){ dump() } 
    
    while( pos < instr.size && !stop ){
      
      // Console.println( instr(pos) ) 
      // Console.println( "pre:"+ registers ) 
      if( debug ){ Console.println( instr( pos ) ) }

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
        case o : Out => {
          val v = o.x match {
            case Left(i) => {i}
            case Right(r) => { registers.get(r(0)).get }
          }
          transmit(v)
          pos = pos + 1
        }
        case _ => { throw new IllegalStateException( "bad instruction") }
      }
      
      // Console.println( "pst:"+ registers ) 
      
      count = count + 1
      
      if( count % 10000 == 0 ){
        // Console.print(".")
      }
      
      if( debug ){ dump() } 
      
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
        Cpy( numOrReg(parts(1)), Right(parts(2)) )
      }
      case "inc" => { Inc( parts(1) ) }
      case "dec" => { Dec( parts(1) ) }
      case "jnz" => { 
        Jnz( numOrReg(parts(1)), numOrReg(parts(2)) )
      }
      case "tgl" => {
        Tgl(parts(1))
      }
      case "out" => {
        Out(numOrReg(parts(1)))
      }
      case _ => { throw new IllegalArgumentException( line ) }
    }
  }
  
  def numOrReg( s : String ) : Either[Int,String] = {
        if( isNumber( s )  ){
          Left(s.toInt)
        }
        else {
          Right(s)
        }
  }
  
  def isNumber( s : String ) : Boolean = {
    s(0) <= 57 
  }
  
  
  class Instruction()
  case class Cpy( x : Either[Int,String], y : Either[Int,String] ) extends Instruction
  case class Inc( x : String ) extends Instruction
  case class Dec( x : String ) extends Instruction
  case class Jnz( x : Either[Int,String], y : Either[Int,String] ) extends Instruction
  case class Tgl( x : String ) extends Instruction
  case class Out( x : Either[Int,String] ) extends Instruction
  
  
}