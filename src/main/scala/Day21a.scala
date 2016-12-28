

import scala.collection.mutable.ListBuffer
import org.apache.commons.io.IOUtils
import java.io.BufferedReader
import java.io.FileReader
import scala.collection.JavaConversions._
import org.apache.commons.lang3.StringUtils
import java.security.MessageDigest
import scala.collection.mutable.HashMap

object Day21a {
  
  class Instruction()
  case class SwapPos( x : Int, y : Int ) extends Instruction
  case class SwapLetter( a : Char, b : Char ) extends Instruction
  case class Rotate( dir : Char, x : Int ) extends Instruction
  case class RotatePos( c : Char ) extends Instruction
  case class Reverse( x : Int, y : Int ) extends Instruction
  case class Move( x : Int, y : Int ) extends Instruction
  
  def main( args : Array[String] ) : Unit = {
    Console.println("day21...")
    
    val lines = read( "./src/day21.txt" )
    
    var steps = lines.map( parse( _ ) )
    steps = steps.reverse
    
    // steps.foreach( Console.println( _ ) )
    
    // val input = "decab"
    // val input = "abcdefgh"
    val input = "fbgdceah"
    
    val output = steps.foldLeft(input)( apply )
    
    Console.println(output)
    
  }
  
  def apply( input : String, step : Instruction ) : String = {
    
    Console.println(step)
    Console.println(input)

    var working = ListBuffer[Char]() ++ input.toList
    
    step match {
      case SwapPos(x,y) => {
        val a = working(x)
        val b = working(y)
        working(x) = b
        working(y) = a
      }
      case SwapLetter(a,b) => {
        val p1 = working.indexOf(a)
        val p2 = working.indexOf(b)
        working(p2) = a
        working(p1) = b
      }
      case Rotate(dir,x) => {
        if( dir == 'l' ){
          working = working.reverse
        }
        
        for( i <- 0 until x ){
          working = working.tail :+ working.head
        }

        if( dir == 'l' ){
          working = working.reverse
        }
      }
      case RotatePos(c) => {
        
        // get the current position of c
        // and use the chart
        
        val pos = working.indexOf(c) match {
          case 1 => 0
          case 3 => 1
          case 5 => 2
          case 7 => 3
          case 2 => 4 
          case 4 => 5 
          case 6 => 6 
          case 0 => 7 
        }
        
        // rotate left until the character is at pos
        
        var current = working.indexOf(c)
        
        while( current != pos ){
          working = working.tail :+ working.head
          current = working.indexOf(c)
        }
        
      }
      case Reverse(x,y) => {
        
        val start = working.slice(0, x)
        val end = working.slice(y+1,working.size)
        
        val mid = working.slice(x, y+1)
        
        working = start ++ mid.reverse ++ end
        
      }
      case Move(x,y) => {
        val c = working.remove(y)
        working.insert(x,c)
      }
    }
    
    
    Console.println(working.mkString)
    working.mkString
  }

  def read( file : String ) : List[String] = {
    val l =  IOUtils.readLines( new BufferedReader( new FileReader( file ) ) )
    l.toList
  }
  
  def parse( line : String ) : Instruction = {
    val parts = line.split(" ")
    parts(0) match {
      case "swap" => {
        parts(1) match {
          case "position" => { SwapPos( parts(2).toInt, parts(5).toInt )}
          case "letter" => { SwapLetter( parts(2)(0), parts(5)(0) ) }
          case _ => { throw new IllegalStateException("invalid step") }
        }
      }
      case "reverse" => { Reverse( parts(2).toInt, parts(4).toInt ) }
      case "rotate" => { 
        parts(1) match {
          case "left" => { Rotate( 'l', parts(2).toInt ) }
          case "right" => { Rotate( 'r', parts(2).toInt ) }
          case "based" => { RotatePos( parts(6)(0) ) }
          case _ => { throw new IllegalStateException("invalid step") }
        }
      }
      case "move" => { Move( parts(2).toInt, parts(5).toInt ) }
      case _ => { throw new IllegalStateException("unknown instruction") }
    }
  }
  
}